
let wmax = 800 
let hmax = 255
let pos  = ref (0, 0)

let spritesheet = ref (Obj.magic 0)
let display     = ref (Obj.magic 0)

let getX pos = match !pos with (x, _) -> x

let sdl_init () =
    begin
        Sdl.init [`EVERYTHING];
        Sdlevent.enable_events Sdlevent.all_events_mask;
    end

let init () =
    sdl_init ();
    spritesheet := Sdlloader.load_image "assets/spritesheet.png";
	display     := Sdlvideo.set_video_mode wmax hmax [`DOUBLEBUF]

let show r1 r2 = 
	Sdlvideo.blit_surface !spritesheet (~src_rect:r1) !display (~dst_rect:r2) ();
	Sdlvideo.flip !display
	

let update  () = ()
let draw    () =
	let r1 = Sdlvideo.rect 0 0 144 255 in
	let r2 = Sdlvideo.rect 0 0 144 255 in
	show r1 r2



let _ = 
    let running = ref true in
    let time    = ref 0    in
    init();
    while !running do
        time := Sdltimer.get_ticks ();
        update();
        draw();
        time := Sdltimer.get_ticks () - !time;
        Sdltimer.delay (1000 / 60 - !time)
    done
