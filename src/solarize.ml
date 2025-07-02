open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let compare_to_threshold ~color threshold ~max_val =
  if Float.compare (Int.to_float color) threshold > 0
  then max_val - color
  else color
;;

let transform image =
  let threshold = 0.4 in
  let max_val = Image.max_val image in
  let thresh_val = Int.to_float (Image.max_val image) *. threshold in
  Image.map image ~f:(fun (r, g, b) ->
    let new_r = compare_to_threshold ~color:r ~max_val thresh_val in
    let new_g = compare_to_threshold ~color:g ~max_val thresh_val in
    let new_b = compare_to_threshold ~color:b ~max_val thresh_val in
    new_r, new_g, new_b)
;;

let command =
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
