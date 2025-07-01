open Core

let error_distribution ~x ~y image (float_error : float) =
  let error = Float.to_int (Float.round float_error) in
  if Image.in_bounds image ~x ~y
  then (
    let old_color, _, _ = Image.get image ~x ~y in
    Image.set
      image
      ~x
      ~y
      (old_color + error, old_color + error, old_color + error))
;;

(* This should look familiar by now! *)
let transform image =
  let gray_image = Grayscale.transform image in
  let max_val = Image.max_val gray_image in
  let max_val = Int.to_float max_val in
  Image.mapi gray_image ~f:(fun ~x ~y pixel ->
    let int_r, _, _ = pixel in
    let r = Int.to_float int_r in
    if Float.compare r (max_val /. 2.0) > 0
    then (
      let error = r -. max_val in
      error_distribution ~x:(x + 1) ~y gray_image (error *. 7. /. 16.);
      error_distribution ~x:(x - 1) ~y:(y + 1) gray_image (error *. 3. /. 16.);
      error_distribution ~x ~y:(y + 1) gray_image (error *. 5. /. 16.);
      error_distribution ~x:(x + 1) ~y:(y + 1) gray_image (error *. 1. /. 16.);
      ( Image.max_val gray_image
      , Image.max_val gray_image
      , Image.max_val gray_image ))
    else (
      let error = r in
      error_distribution ~x:(x + 1) ~y gray_image (error *. 7. /. 16.);
      error_distribution ~x:(x - 1) ~y:(y + 1) gray_image (error *. 3. /. 16.);
      error_distribution ~x ~y:(y + 1) gray_image (error *. 5. /. 16.);
      error_distribution ~x:(x + 1) ~y:(y + 1) gray_image (error *. 1. /. 16.);
      0, 0, 0))
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

let%expect_test "dither" =
  let input_file = "/home/ubuntu/raster/images/beach_portrait.ppm" in
  let reference_file =
    "/home/ubuntu/raster/images/reference-beach_portrait_dither.ppm"
  in
  let num_different =
    Image.compare_files
      ~input_file
      ~test_func:transform
      ~reference:reference_file
  in
  print_s [%message (num_different : int)];
  [%expect "(num_different 0)"]
;;
