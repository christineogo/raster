open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  (* image *)
  Image.map image ~f:(fun (r, g, b) ->
    (r + b + g) / 3, (r + b + g) / 3, (r + b + g) / 3)
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

(* let%expect_test "grayscale" =
   let same_image =
   Image.for_all2
   (transform
   (Image.load_ppm
   ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm"))
   (Image.load_ppm
   ~filename:
   "/home/ubuntu/raster/images/reference-beach_portrait_gray.ppm")
   ~f:(fun a b -> Pixel.equal a b)
   in
   print_s [%message (same_image : bool)];
   [%expect "(same_image true)"]
   ;; *)

let%expect_test "grayscale" =
  let input_file = "/home/ubuntu/raster/images/beach_portrait.ppm" in
  let reference_file =
    "/home/ubuntu/raster/images/reference-beach_portrait_gray.ppm"
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

(* let%expect_test "grayscale" =
   print_endline *)
