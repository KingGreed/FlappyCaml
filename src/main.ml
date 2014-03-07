
let wmax = 800 
let hmax = 240
let pos  = ref (0, 0)
let state = ref 0
let t = ref 0.

let progress = ref 0		(*avancement*)	
let pipes = Queue.create()
let nb_pipe = 10
let space_pipe = 200
let next_x_pipe = ref 500

let foi = float_of_int
let iof = int_of_float


let spritesheet = ref (Obj.magic 0)
let display     = ref (Obj.magic 0)

let getX pos = match pos with (x, _) -> x
let getY pos = match pos with (_, y) -> y

let sdl_init () =
    begin
        Sdl.init [`EVERYTHING];
        Sdlevent.enable_events Sdlevent.all_events_mask;
    end

let gen_pipe() =
	Queue.push (!next_x_pipe, Random.int 400 + 200) pipes;
	next_x_pipe := !next_x_pipe + space_pipe

let init () =
	Random.self_init();
    sdl_init ();

    spritesheet := Sdlloader.load_image "assets/spritesheet.png";
	display     := Sdlvideo.set_video_mode wmax hmax [`DOUBLEBUF];
	for i = 0 to nb_pipe do
		gen_pipe()
	done

let show r1 r2 = 
	let d = Sdlvideo.display_format ~alpha:true !spritesheet in
	Sdlvideo.blit_surface ~src:d 
		~src_rect:r1 ~dst:!display 
		~dst_rect:r2  ()
	

let update  () =  let (x, y) = !pos in 
    pos := (x+1, iof(sin(!t) *. 100. +. 100.));
    t := !t +. 0.05;
    state := (!state + 1) mod 3;
	if (getX (Queue.peek pipes) - !progress < -50) then
		ignore (Queue.pop pipes); 
	if (Queue.length pipes < nb_pipe) then
		gen_pipe();
	incr progress
	


let draw    () =
    (* background display *)
    (* Sdlvideo. *)
    let (posX, posY) = !pos in 
    for i = 0 to (800 / 144) + 1 do
        let r1 = Sdlvideo.rect 0 0 144 255 in
        let r2 = Sdlvideo.rect (i * 144 - (posX mod 144)) (posY / 50 - 15) 144 255 in
        show r1 r2
    done;

    let x = ref 223 in 
    let y = ref 124 in

    if (!state = 0)      then ( x := 223; y := 124; )
    else if (!state = 1) then ( x := 264; y := 64;  )
    else if (!state = 2) then ( x := 264; y := 90;  )
    else                      ( x := 264; y := 64;  );


    let r1 = Sdlvideo.rect !x   !y 17 17 in
	let r2 = Sdlvideo.rect 20 posY  17 17 in
	show r1 r2;

    Sdlvideo.flip !display

let _ = 
    let running = ref true in
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
