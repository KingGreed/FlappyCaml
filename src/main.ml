let pos  = ref (0, 0)
let state = ref 0
let t = ref 0.

let foi = float_of_int
let iof = int_of_float

(* Sprites dimensions *)
let pipeHeight    = 134
let pipeHeadSrc   = (302, 123, 26, 12) (* x, y, w, h *)
let pipeCorpseSrc = (303, 0, 23, 122)  (* x, y, w, h *) 

let nb_pipe     = 10 (* should be a function of screen width *)
let next_x_pipe = ref 500

let offset = ref 100 (* Size in pixel to first pipe *)
let spaceYBetweenPipe = 40
let spaceXBetweenPipe = 80
let progress = ref 0

let pipes = Queue.create()
let pipesArray = [76; 100;]

let backgroundWidth  = 144
let backgroundHeight = 255

let wmax = backgroundWidth * 2 
let hmax = 240

let minPipeHeight = hmax - spaceYBetweenPipe - pipeHeight
let maxPipeHeight = pipeHeight

let spritesheet = ref (Obj.magic 0)
let display     = ref (Obj.magic 0)

let getX pos = match pos with (x, _) -> x
let getY pos = match pos with (_, y) -> y

(* Init *)
let gen_pipe() =
    Queue.push (!next_x_pipe, Random.int 400 + 200) pipes;
    next_x_pipe := !next_x_pipe + spaceXBetweenPipe

let sdl_init () =
    begin
        Sdl.init [`EVERYTHING];
        Sdlevent.enable_events Sdlevent.all_events_mask;
    end

let init () =
    Random.self_init();
    sdl_init ();

    spritesheet := Sdlloader.load_image "assets/spritesheet.png";
	display     := Sdlvideo.set_video_mode wmax hmax [`DOUBLEBUF];
    for i = 0 to nb_pipe do
        gen_pipe()
    done

    
(* Update *)
let update  () =  let (x, y) = !pos in 
    pos := (x+1, iof(sin(!t) *. 100. +. 100.));
    t := !t +. 0.05;
    state := (!state + 1) mod 3;
    if (getX (Queue.peek pipes) - (x+1) < -50) then
        ignore (Queue.pop pipes); 
    if (Queue.length pipes < nb_pipe) then
        gen_pipe()


(* Draw *)
let show r1 r2 = 
	let d = Sdlvideo.display_format ~alpha:true !spritesheet in
	Sdlvideo.blit_surface ~src:d 
		~src_rect:r1 ~dst:!display 
		~dst_rect:r2  ()

let draw    () =
    (* background display *)
    (* Sdlvideo. *)
    let (posX, posY) = !pos  in
    let w = backgroundWidth  in
    let h = backgroundHeight in
    for i = 0 to (wmax / w) do
        let r1 = Sdlvideo.rect 0 0 w h in
        let r2 = Sdlvideo.rect (i * w - (posX mod w)) (posY / 50 - 15) w h in
        show r1 r2
    done;

    let no = ref 0 in
    List.iter (fun h -> 
        let posX       = !offset + spaceXBetweenPipe * !no in
        incr no;


        (* Pipe Head *)
        let (srcX, srcY, srcW, srcH)    = pipeHeadSrc       in
        let (dstX, dstYTop, dstYBottom) = (posX, h - srcH, 
                h + spaceYBetweenPipe)                      in
        let r1 = Sdlvideo.rect srcX srcY srcW srcH          in
        let r2 = Sdlvideo.rect dstX dstYTop srcW srcH       in
        let r3 = Sdlvideo.rect dstX dstYBottom srcW srcH    in
        show r1 r2; (* Top    pipe head *)
        show r1 r3; (* Bottom pipe head *)


        (* Pipe corpse *)
        let headHeight               = srcH                              in
        let topCorpseHeight          = h - headHeight                    in
        let botomCorpseHeight        = hmax - spaceYBetweenPipe - h      in
        let dstX                     = posX + 1                          in
        let (srcX, srcY, srcW, srcH) = pipeCorpseSrc                     in

        (* Top corpse *)
        let (dstYTop, srcH) = (0, topCorpseHeight)               in
        let dstYBot         = h + spaceYBetweenPipe + headHeight in

        let r1        = Sdlvideo.rect srcX srcY srcW srcH    in
        let r2        = Sdlvideo.rect dstX dstYTop srcW srcH in
        show r1 r2;

        (* Bottom corpse *)
        let srcH      = botomCorpseHeight in
        let r1        = Sdlvideo.rect srcX srcY srcW srcH    in
        let r2        = Sdlvideo.rect dstX dstYBot srcW srcH in
        show r1 r2

    ) pipesArray;

    let x = ref 223 in 
    let y = ref 124 in


    let camelXdimensions = 17 in
    let camelYdimensions = 17 in
    let pX   = 20 in

    let r1 = Sdlvideo.rect !x   !y camelXdimensions camelYdimensions in
	let r2 = Sdlvideo.rect pX posY camelXdimensions camelYdimensions in
	show r1 r2;

    if(!state <> 2) then Sdlvideo.flip !display

let running = ref true

let _ = 
    let time    = ref 0    in
    init();
    while !running do
        time := Sdltimer.get_ticks ();
        update();
        draw();
        time := Sdltimer.get_ticks () - !time;
        time := 1000 / 60 - !time;
        if(!time > 0) then (
            Sdltimer.delay (!time)
        )
    done
