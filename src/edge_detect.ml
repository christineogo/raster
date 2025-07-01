open Core

let x_gradient ~x ~y image =
  let points =
    [ x - 1, y - 1, -1
    ; x + 1, y - 1, 1
    ; x - 1, y, -2
    ; x + 1, y, 2
    ; x - 1, y + 1, -1
    ; x + 1, y + 1, 1
    ]
  in
  List.fold points ~init:0 ~f:(fun acc point ->
    let point_x, point_y, multiplier = point in
    if Image.in_bounds ~x:point_x ~y:point_y image
    then (
      let magnitude, _, _ = Image.get ~x:point_x ~y:point_y image in
      acc + (multiplier * magnitude))
    else acc)
;;

let y_gradient ~x ~y image =
  let points =
    [ x - 1, y - 1, -1
    ; x + 1, y - 1, -1
    ; x, y - 1, -2
    ; x - 1, y + 1, 1
    ; x, y + 1, 2
    ; x + 1, y + 1, 1
    ]
  in
  List.fold points ~init:0 ~f:(fun acc point ->
    let point_x, point_y, multiplier = point in
    if Image.in_bounds ~x:point_x ~y:point_y image
    then (
      let magnitude, _, _ = Image.get ~x:point_x ~y:point_y image in
      acc + (multiplier * magnitude))
    else acc)
;;

let transform image =
  let new_image = Grayscale.transform (Blur.transform image ~radius:2) in
  Image.mapi new_image ~f:(fun ~x ~y _pixel ->
    let convolution_square =
      (x_gradient ~x ~y new_image * x_gradient ~x ~y new_image)
      + (y_gradient ~x ~y new_image * y_gradient ~x ~y new_image)
    in
    let float_convolution_square = Int.to_float convolution_square in
    (* let raw_pixel =
       Float.to_int (Float.round (Float.sqrt float_convolution_square))
       in *)
    if
      Float.compare
        (Float.sqrt float_convolution_square)
        (0.4 *. Int.to_float (Image.max_val new_image))
      > 0
    then
      ( Image.max_val new_image
      , Image.max_val new_image
      , Image.max_val new_image )
    else 0, 0, 0)
;;

(* let new_pixel = min (max 0 raw_pixel) (Image.max_val new_image) in
   new_pixel, new_pixel, new_pixel) *)

let _command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let command =
  Command.basic
    ~summary:"Edge detect an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_edge_detect.ppm")]
;;
