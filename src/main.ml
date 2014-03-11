let t = ref 0.

let int = int_of_float

let tau = 8. *. atan 1.

type vector2d = {
    mutable x : int;
    mutable y : int;
}

(*type box2d = {
    mutable x : int;
    mutable y : int;
    mutable w : int;
    mutable h : int;
}*)

type gamestate = {
    mutable pos : vector2d;
    mutable yvelocity : float;
    mutable next_pipe : int;
    mutable alive : bool;
    mutable frame : int;
}

let state = {
    pos = { x = 0; y = 0};
    (*box = { x = 0; y = 0; w = 17; h = 17};*)
    yvelocity = 0.;
    next_pipe = 0;
    alive = false;
    frame = 0;
}

let screen = { x = 280; y = 240 }

(* Physics *)
let g = 10.0
let step = 0.09

(* Graphics constants *)
let spritesheet = ref (Obj.magic 0)
let display     = ref (Obj.magic 0)

let background = Sdlvideo.rect   0   0 144 255
let upper_pipe = Sdlvideo.rect 302   0  26 135
let lower_pipe = Sdlvideo.rect 330   0  26 129
let camel      = Sdlvideo.rect 223 124  17  17

let nb_pipe = 3

let offset = 100 (* Size in pixel to first pipe *)
let padding = { x = 85; y = 40 }

let pipes = Queue.create()

(* Init *)
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
    state.pos       <- { x = 0; y = 0};
    state.yvelocity <- 0.;
    state.next_pipe <- offset;
    state.alive     <- true;
    state.frame     <- 0;
    for i = 0 to nb_pipe do
        push_pipe()
    done

let init () =
    Random.self_init();
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
    spritesheet := Sdlloader.load_image "assets/spritesheet.png";
    display     := Sdlvideo.set_video_mode screen.x screen.y [`DOUBLEBUF];

    new_game()

(* Update *)
let pipe_out p = p + upper_pipe.Sdlvideo.r_w - state.pos.x < 0

(*let get_bs_from_p p = ({ x = p.x - state.pos.x, y = 0, w = 26, h = p.y },
                       { x = p.x - state.pos.x, y = p.y, w = 26, h = screen.y - p.y})

let intersect b1 b2 = b1.x + b1.w < b2.x || b1.y + b1.h < b2.y || b2.y + b2.h < b1.y

let update_box () =
    state.box.x <- state.pos.x;
    state.box.y <- state.pos.y*)

let jump () = state.yvelocity <- -20.

let update  () =
    if state.pos.y > 100 then jump ();
    state.frame <- (state.frame + 1) mod 3;
    state.yvelocity <- g *. step +. state.yvelocity;
    state.pos.y <- (int) (step *. (2. *. state.yvelocity +. g *. step)) + state.pos.y;
    state.pos.x <- state.pos.x + 1;
    (*update_box ();
    let (b_up, b_down) = get_bs_from_p (Queue.peek pipes) in
    if intersect state.box b_up || intersect state.box b_down then gamestate.alive <- false;*)
    if pipe_out (Queue.peek pipes).x then cycle_pipes ()

(* Draw *)
let show r1 r2 =
    let d = Sdlvideo.display_format ~alpha:true !spritesheet in
    Sdlvideo.blit_surface
        ~src:d
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

    showat camel (20, state.pos.y);

    Sdlvideo.flip !display

let running = ref true

let _ =
    let time = ref 0 in
    init();
    while !running do
        time := Sdltimer.get_ticks ();
        update();
        draw();
        time := Sdltimer.get_ticks () - !time;
        time := 1000 / 60 - !time;
        if !time > 0 then (
            Sdltimer.delay (!time)
        )
    done
