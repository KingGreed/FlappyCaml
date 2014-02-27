let sdl_init () =
    begin
        Sdl.init [`EVERYTHING];
        Sdlevent.enable_events Sdlevent.all_events_mask;
    end

let spritesheet = ref Obj.magic 0
let pos = ref (0, 0)
let getX pos = match !pos with (x, _) -> x




let init () =
    sdl_init ();
    spritesheet := Sdlloader.load_image "assets/spritesheet.png"

let _ = 
    let running = ref true;
    let time = ref 0 in
    init();
    while !running do
        time := Sdltimer.get_ticks ();
        update();
        draw();
        time := Sdltimer.get_ticks () - !time;
        Sdltimer.delay (1000 / 60 - !time)
    done;
    release()
