let wmax = 800 
let hmax = 240
let pos  = ref (0, 0)
let state = ref 0
let t = ref 0.

let foi = float_of_int
let iof = int_of_float


let pipeHeadSrc   = (302, 123, 26, 12) (* x, y, w, h *)
let pipeCorpseSrc = (303, 0, 23, 122)  (* x, y, w, h *) 

let downPipeSrc = (330, 0)
let pipeWidth   = 26
let nb_pipe     = 10 (* should be a function of screen width *)
let next_x_pipe = ref 500

let offset = ref 1 (* Size in pixel to first pipe *)
let spaceYBetweenPipe = 40
let spaceXBetweenPipe = 80
let progress = ref 0

let pipes = Queue.create()
let pipesArray = [50; 60; 140]


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
    let (posX, posY) = !pos in 
    for i = 0 to (800 / 144) + 1 do
        let r1 = Sdlvideo.rect 0 0 144 255 in
        let r2 = Sdlvideo.rect (i * 144 - (posX mod 144)) (posY / 50 - 15) 144 255 in
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
        let r1                       = Sdlvideo.rect srcX srcY srcW srcH in

        (* Top corpse *)
        let chDivSrcH = topCorpseHeight / srcH               in
        for i = 0 to chDivSrcH - 1 do
            let dstYTop = 0 + i * srcH                       in 
            let r2 = Sdlvideo.rect dstX dstYTop srcW srcH   in
            show r1 r2
        done;
        if topCorpseHeight mod srcH <> 0 then (
            let dstYTop = 0 + chDivSrcH * srcH               in
            let srcH    = topCorpseHeight - chDivSrcH * srcH in
            let r1 = Sdlvideo.rect srcX srcY srcW srcH       in
            let r2 = Sdlvideo.rect dstX dstYTop srcW srcH    in
            show r1 r2
        );

        (* Bottom corpse *)
        let chDivSrcH = botomCorpseHeight / srcH           in
        let dstY = h + spaceYBetweenPipe + headHeight      in
        for i = 0 to chDivSrcH + 1 do
            let dstYBot = dstY + i * srcH                  in 
            let r2 = Sdlvideo.rect dstX dstYBot srcW srcH  in
            show r1 r2
        done;

    ) pipesArray;

    let x = ref 223 in 
    let y = ref 124 in

    if (!state = 0)      then ( x := 223; y := 124; )
    else if (!state = 1) then ( x := 264; y := 64;  )
    else if (!state = 2) then ( x := 264; y := 90;  )
    else                      ( x := 264; y := 64;  );


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
