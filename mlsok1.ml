type direction =
        | Up
        | Right
        | Down 
        | Left 
;;

type unmovable_level_data =
        | Wall
        | Floor
        | Goal
;;

type box =
        | Pos of int * int
;;

type level = {
        layout: unmovable_level_data array array;
        pos: int * int;
        boxes: box list;
};;

type event =
        | Direction of direction
        | Quit
        | Continue
        | Undo
;;

type state = {
        level_state: level;
        moves: int;
        pushes: int;
};;

type condition =
        | Push of level
        | Move of level
        | No_move
;;

type key =
        | Key of int
;;

type key_binding = {
        key: key;
        event: event;
};;

exception Unknown_element;;
exception Print_error;;
exception Impossible_state
exception Init_error

(*
 * Load a level from a list of strings
 * The strings must be of the same length
 * Returns level
 *)
let level_from_list str_list =
        let layout = Array.make_matrix (List.length str_list) (String.length (List.hd str_list)) Wall
        and pos = ref (0, 0)
        and boxes = ref [] in

        let rec go str y =
                match str with
                | [] -> {layout = layout; pos = !pos; boxes = !boxes}
                | head :: tail ->
                                for x = 0 to String.length head - 1 do
                                        match head.[x] with
                                        | '#' -> layout.(y).(x) <- Wall
                                        | ' ' -> layout.(y).(x) <- Floor
                                        | '.' -> layout.(y).(x) <- Goal
                                        | '@' -> pos := x, y; layout.(y).(x) <- Floor
                                        | '$' -> boxes := Pos(x, y) :: !boxes; layout.(y).(x) <- Floor
                                        | _   -> raise Unknown_element
                                done;
                                go tail (y + 1)
        in
        go str_list 0
;;

(*
 * Return integer value of character associated with an element
 * Curses expects all characters to be an integer
 * so when printing, integers need to be used
 *)
let int_of_element elem =
        int_of_char (
                match elem with
                | Wall   -> '#'
                | Floor  -> ' '
                | Goal   -> '.'
        )
;;

(*
 * Helper function for printing characters
 * f: function to print
 * ch: character or string in format that f expects
 *)
let print f ch =
        if f ch = false then
                raise Print_error
        else
                ()
;;

(*
 * Given a level, print the level in window win
 * Also print the number of moves and pushes
 *)
let print_level l win moves pushes =
        if (Curses.wmove win 0 0) = false then
                raise Print_error;

        (* Print level data that is unmovable *)
        Array.iter (fun i ->
                Array.iter (fun j ->
                        print (Curses.waddch win) (int_of_element j))
                i; print (Curses.waddch win) (int_of_char '\n')
        ) l.layout;
        (* Print the moves and pushes now since the cursor will be in the right place *)
        print (Curses.waddstr win) ("Moves: " ^ (string_of_int moves) ^ " Pushes: " ^ (string_of_int pushes) ^ "\n");

        (* Print boxes *)
        List.iter (fun (Pos(x, y)) ->
                match l.layout.(y).(x) with
                | Goal -> print (Curses.mvwaddch win y x) (int_of_char '*')
                | Floor -> print (Curses.mvwaddch win y x) (int_of_char '$')
                | Wall -> raise Impossible_state
        ) l.boxes;

        (* Print character *)
        let x, y = l.pos in
        print (Curses.mvwaddch win y x) (int_of_char '@');

        ignore (Curses.wnoutrefresh win);
        ignore (Curses.doupdate ())
;;

let level_test =
        ["    #####          ";
         "    #   #          ";
         "    #$  #          ";
         "  ###  $##         ";
         "  #  $ $ #         ";
         "### # ## #   ######";
         "#   # ## #####  ..#";
         "# $  $          ..#";
         "##### ### #@##  ..#";
         "    #     #########";
         "    #######        "]
;;

(*
 * Key binding hash table
 * Maps integer values of keys to key_binding
 *)
let key_bindings = Hashtbl.create 10;;

(*
 * Helper function for binding keys
 *)
let bind_key key' event' =
        Hashtbl.add key_bindings key' ({key = Key(key'); event = event'})
;;

bind_key (Char.code 'q') Quit;;
bind_key (Char.code 'w') Quit;;
bind_key (Char.code 'u') Undo;;
bind_key Curses.Key.left (Direction(Left));;
bind_key Curses.Key.down (Direction(Down));;
bind_key Curses.Key.right (Direction(Right));;
bind_key Curses.Key.up (Direction(Up));;

(*
 * Return the event associated with a particular key pressed
 * Return Continue if no event associated with key
 *)
let get_key () =
        let k = Curses.wgetch (Curses.stdscr ()) in

        try
                (Hashtbl.find key_bindings k).event
        with
        | Not_found -> Continue
;;

(*
 * Given the coordinates of a box, move the box in the specified direction
 * boxes: list of boxes
 * x: x coordinate of box to move
 * y: y coordinate of box to move
 * xdir: x direction of movement
 * ydir: y direction of movement
 * Return: new list of boxes
 *)
let move_box boxes x y xdir ydir =
        List.fold_left (fun acc box ->
                let Pos(boxx, boxy) = box in
                if boxx = x && boxy = y then
                        Pos(x + xdir, y + ydir) :: acc
                else
                        box :: acc
        ) [] boxes
;;

(*
 * Check to see if box exists at coordinate x, y
 * boxes: list of boxes
 * x: x coordinate
 * y: y coordinate
 * Return: boolean
 *)
let box_exists boxes x y =
        List.exists (fun (Pos(boxx, boxy)) -> boxx = x && boxy = y) boxes
;;

(*
 * Check to see if all boxes are on goals (and therefore level is complete)
 * boxes: list of boxes
 * layout: level layout of unmovable elements
 * Return: boolean
 *)
let is_level_won boxes layout =
        List.for_all (fun (Pos(x, y)) -> layout.(y).(x) = Goal) boxes
;;

(*
 * Move the movable elements in the specified direction
 * l: level
 * xdir: x direction
 * ydir: y direction
 * Return: condition
 *)
let move l xdir ydir =
        let x, y = l.pos in

        match l.layout.(y + ydir).(x + xdir) with
        (*
         * Moving a box when on a floor or goal piece is essentially the same logic
         * Need to check 2 places because that will be the new location of the box
         *)
        | Floor | Goal when (box_exists l.boxes (x + xdir) (y + ydir)) -> (
                        match l.layout.(y + (2 * ydir)).(x + (2 * xdir)) with
                        (* No box blocking move *)
                        | Floor | Goal when not (box_exists l.boxes (x + (2 * xdir)) (y + (2 * ydir))) ->
                                        Push( {
                                                boxes = move_box l.boxes (x + xdir) (y + ydir) xdir ydir;
                                                pos = x + xdir, y + ydir;
                                                layout = l.layout
                                                } )
                        (* Box blocking move *)
                        | Floor | Goal ->
                                        No_move
                        (* Wall blocking move *)
                        | Wall -> No_move
        )
        (* No box needs to be moved *)
        | Floor | Goal ->
                        Move( {
                                boxes = l.boxes;
                                pos = x + xdir, y + ydir;
                                layout = l.layout;
                        } )
        (* Wall prevents movement *)
        | Wall -> No_move
;;

let move_left l =
        move l (0 - 1) 0
;;

let move_down l =
        move l 0 (0 + 1)
;;

let move_up l =
        move l 0 (0 - 1)
;;

let move_right l =
        move l (0 + 1) 0
;;

(*
 * The level is complete so print level complete message, wait for key press and quit
 * win: window
 *)
let level_complete win =
        let y, x = Curses.getmaxyx win in

        ignore (Curses.mvwaddstr win (y / 2) (x / 2) "Level complete\n");
        ignore (Curses.wnoutrefresh win);
        ignore (Curses.doupdate ());
        ignore (Curses.getch ());
        ()
;;

(*
 * Main program loop
 * states': list state, first state
 * win: window
 *)
let run states' win =
        (*
         * Get input from user and act on event
         * states: list state, all game states up to that point
         *)
        let rec get_input states =
                try
                        let s   = List.hd states in
                        let lev = s.level_state in

                        print_level lev win s.moves s.pushes;

                        if is_level_won lev.boxes lev.layout then
                                level_complete win
                        else (
                                match get_key () with
                                | Direction(Left) -> next_state (move_left lev) states
                                | Direction(Down) -> next_state (move_down lev) states
                                | Direction(Up) -> next_state (move_up lev) states
                                | Direction(Right) -> next_state (move_right lev) states
                                | Quit -> ()
                                | Undo -> (
                                                try
                                                        get_input (List.tl states)
                                                with
                                                | _ -> get_input states
                                )
                                | Continue -> get_input states
                        )
                with
                | Invalid_argument _ -> get_input states
        (*
         * Generate the next game state
         * f: function that returns new state
         * states: all states generated so far
         *)
        and next_state f states =
                let s = List.hd states in

                match f with
                | Push(l1) -> get_input ({level_state = l1;
                                          moves = s.moves + 1;
                                          pushes = s.pushes + 1;
                                         } :: states)
                | Move(l1) -> get_input ({level_state = l1;
                                          moves = s.moves + 1;
                                          pushes = s.pushes;
                                         } :: states)
                | No_move -> get_input states
        in
        ignore (Curses.refresh ());
        get_input states'
;;

(*
 * Helper function for initialising curses
 * f: Initialisation function
 *)
let init_func f =
        if f = false then
                raise Init_error
        else
                ()
;;

let () =
        let l = level_from_list level_test in

        (*
         * Make new play area window
         * Return: window
         *)
        let make_play_win () =
                let x = Array.length l.layout.(0)
                and y = Array.length l.layout 
                and y1, x1 = Curses.getmaxyx (Curses.stdscr ()) in

                (* Get roughly in the middle of the display *)
                Curses.newwin (y + 2) (if x < 32 then 32 else x + 1) ((y1 / 2) - (y / 2)) ((x1 / 2) - ((if x < 32 then 32 else x) / 2))
        in

        (* Initial state *)
        let init_state =
                {level_state = l;
                 moves = 0;
                 pushes = 0;
                }
        in

        (*
         * Initialise cursors
         *)
        let init_cursors () =
                let _ = Curses.initscr () in

                let win = make_play_win () in

                init_func (Curses.cbreak ());
                init_func (Curses.keypad (win) true);
                init_func (Curses.keypad (Curses.stdscr ()) true);
                init_func (Curses.nodelay (win) false);
                init_func (Curses.nodelay (Curses.stdscr()) false);
                init_func (Curses.curs_set 0);
                init_func (Curses.noecho ());

                Curses.leaveok (win) true;
                Curses.timeout (-1);
                win
        in

        try
                let win = init_cursors () in

                run [init_state] win;

                init_func (Curses.delwin win);
                Curses.endwin ();
                Printf.printf "Quit\n"
        with
        | Print_error -> Curses.endwin (); Printf.printf "Print_error\n"
        | Init_error -> Printf.printf "Init_error\n"

;;
