external network_init : unit -> unit = "network_init"
external network_send_state : 'a -> unit = "send_state"

let int = int_of_float

let tau = 8. *. atan 1.

type vector2d = {
    mutable x : int;
    mutable y : int;
}

type box2d = {
    mutable bx : int;
    mutable by : int;
    mutable bw : int;
    mutable bh : int;
}

type gamestate = {
    mutable pos : vector2d;
    mutable yvelocity : float;
    mutable next_pipe : int;
    mutable alive : bool;
    mutable frame : int;
}

let state = {
    pos = { x = 0; y = 0};
    yvelocity = 0.;
    next_pipe = 0;
    alive = false;
    frame = 0;
}

let screen = { x = 1200; y = 240 }

(* Physics *)
let g = 12.0
let step = 0.1

(* autoplay *)
let autoplay = ref false

(* Graphics constants *)
let spritesheet = ref (Obj.magic 0)
let display     = ref (Obj.magic 0)

let background = Sdlvideo.rect   0   0 144 255
let upper_pipe = Sdlvideo.rect 302   0  26 135
let lower_pipe = Sdlvideo.rect 330   0  26 129
let camel      = Sdlvideo.rect 223 124  17  14

let nb_pipe = 20

let camel_margin = 50
let offset = 300 (* Size in pixel to first pipe *)
let padding = { x = 85; y = 40 }

let pipes = Queue.create ()

(* Events *)

let keybinds = Hashtbl.create 10
let bind_key = Hashtbl.add keybinds
let unbind_key = Hashtbl.remove keybinds
let call_key_handler k = Hashtbl.(if mem keybinds k then find keybinds k ())

let pump () = Sdlevent.(match poll () with
    | None   -> ()
    | Some e -> (
        match e with
            | KEYDOWN ke -> call_key_handler ke.keysym
            | _ -> ()
    )
)

(* Init *)
let exit () = exit 0

let push_pipe () = Sdlvideo.(
    let min_y = screen.y - padding.y / 2 - lower_pipe.r_h in
    let range_y = upper_pipe.r_h + padding.y / 2 - min_y in
    let hrange = (float) range_y /. 2. in
    let rand = hrange *. sin (Random.float tau) +. hrange in
    Queue.push {
        x = state.next_pipe;
        y = (int) rand + min_y
    } pipes;
    state.next_pipe <- state.next_pipe + padding.x
)

let cycle_pipes () =
    ignore (Queue.pop pipes);
    push_pipe()

let new_game () =
    state.pos       <- { x = 0; y = screen.y / 2};
    state.yvelocity <- 0.;
    state.next_pipe <- offset;
    state.alive     <- true;
    state.frame     <- 0;
    Queue.clear pipes;
    for i = 0 to nb_pipe do
        push_pipe()
    done

let jump () = state.yvelocity <- -20.

let toggle_autoplay () = autoplay := not !autoplay

let init () =
    Random.self_init();
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
    display     := Sdlvideo.set_video_mode screen.x screen.y [`DOUBLEBUF];
    spritesheet := Sdlloader.load_image "assets/spritesheet.png";
    spritesheet := Sdlvideo.display_format ~alpha:true !spritesheet;

    bind_key Sdlkey.KEY_ESCAPE exit;
    bind_key Sdlkey.KEY_SPACE jump;
    bind_key Sdlkey.KEY_p toggle_autoplay;

    network_init ();

    new_game()

(* Mechanics *)

let jump () = if abs_float state.yvelocity > 0.1 then state.yvelocity <- -20.

let jumpp p = let xcamel = state.pos.x + camel_margin and ylower = p.y + padding.y in if xcamel < p.x - 40 then (if state.pos.y >= ylower + 10 then jump (); ) else (if state.pos.y >= ylower - 20 then jump (); )

let get_second_pipe () =
    let elem = ref { x = 0; y = 0 } in
    let i = ref 0 in
    (try (
        Queue.iter (fun e ->
            if !i = 1 then (elem := e; raise (Failure ""));
            incr i
        ) pipes
    ) with _ -> ());
    !elem

let network_send_states () = let next_pipe = Queue.peek pipes in if state.pos.x + camel_margin < next_pipe.x + upper_pipe.Sdlvideo.r_w then jumpp next_pipe else jumpp (get_second_pipe ())

(* Update *)
let pipe_out p = p + upper_pipe.Sdlvideo.r_w - state.pos.x < 0

let collides_with box = Sdlvideo.(
       state.pos.x + camel_margin < box.bx + box.bw
    && state.pos.y < box.by + box.bh
    && box.bx < state.pos.x + camel_margin + camel.r_w
    && box.by < state.pos.y + camel.r_h
)

let get_pipe_hitboxes p = Sdlvideo.(
    let upper_y = p.y - upper_pipe.r_h - padding.y in
    let lower_y = p.y + padding.y in (
        { bx = p.x; by = upper_y; bw = upper_pipe.r_w; bh = upper_pipe.r_h },
        { bx = p.x; by = lower_y; bw = lower_pipe.r_w; bh = lower_pipe.r_h }
    )
)

let collides_with_pipe p = let (upper,lower) = get_pipe_hitboxes p in
    collides_with upper || collides_with lower


let bird_out () = state.pos.y + camel.Sdlvideo.r_w < 0 || state.pos.y > screen.y

let update  () =
    let next_pipe = Queue.peek pipes in

    if !autoplay then network_send_states ();

    state.frame <- (state.frame + 1) mod 3;
    state.yvelocity <- g *. step +. state.yvelocity;
    state.pos.y <- (int) (step *. (2. *. state.yvelocity +. g *. step)) + state.pos.y;
    state.pos.x <- state.pos.x + 2;
    if collides_with_pipe next_pipe || bird_out () then state.alive <- false;
    if pipe_out next_pipe.x then cycle_pipes ();

    if not state.alive then new_game ()

(* Draw *)
let show r1 r2 =
    Sdlvideo.blit_surface
        ~src:!spritesheet
        ~src_rect:r1
        ~dst:!display
        ~dst_rect:r2
    ()

let showat r (x,y) = show r Sdlvideo.(rect x y r.r_w r.r_h)

let draw () =
    (* background display *)
    let (bw,bh) = Sdlvideo.(background.r_w, background.r_h) in
    for i = 0 to (screen.x / bw) + 1 do
        showat background (i * bw - (state.pos.x mod bw), state.pos.y / 50 - 15)
    done;

    Queue.iter (fun p ->
        let x = p.x - state.pos.x in
        let upper_y = p.y - upper_pipe.Sdlvideo.r_h - padding.y in
        let lower_y = p.y + padding.y in
        showat upper_pipe (x, upper_y);
        showat lower_pipe (x, lower_y);
    ) pipes;

    showat camel (camel_margin, state.pos.y);

    Sdlvideo.flip !display

let running = ref true

let _ =
    let time = ref 0 in
    init();
    Sdltimer.delay 10;
    while !running do
        time := Sdltimer.get_ticks ();
        pump();
        update();
        draw();
        time := Sdltimer.get_ticks () - !time;
        time := 1000 / 60 - !time;
        if !time > 0 then (
            Sdltimer.delay (!time)
        )
    done
