open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  let img_width = Image.width image in
  let img_height = Image.height image in
  let new_image = Image.copy image in
  Image.mapi new_image ~f:(fun ~x ~y _pixel ->
    let min_x = max (x - radius) 0 in
    (* print_s [%message (min_x : int)]; *)
    let max_x = min (img_width - 1) (x + radius) in
    let min_y = max (y - radius) 0 in
    let max_y = min (img_height - 1) (y + radius) in
    let slice =
      Image.slice
        image
        ~x_start:min_x
        ~x_end:max_x
        ~y_start:min_y
        ~y_end:max_y
    in
    Image.mean_pixel slice)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

let%expect_test "grayscale" =
  let same_image =
    Image.for_all2
      (transform
         (Image.load_ppm
            ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm")
         ~radius:3)
      (Image.load_ppm
         ~filename:
           "/home/ubuntu/raster/images/reference-beach_portrait_gray.ppm")
      ~f:(fun a b -> Pixel.equal a b)
  in
  print_s [%message (same_image : bool)];
  [%expect "(same_image true)"]
;;
