let day = 1


(* Prebiranje datotek - vzeto iz vzorcne datoteke project_windows.ms*)
let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l
  
  let lines = String.split_on_char '\n'
end


(* Funkcije *)
let rec najdi_drugo_stevilo n vsota list = match list with
    | [] -> 0
    | x :: xs -> (
        if x + n = vsota then x
        else (najdi_drugo_stevilo n vsota xs)
    )

let rec najdi_par vsota list = match list with
    | [] -> 0
    | x :: xs -> (
        if (najdi_drugo_stevilo x vsota xs) = 0 then (najdi_par vsota xs)
        else x * (najdi_drugo_stevilo x vsota xs)
    )

let naloga1 data = 
  let lines = List.lines data in
  lines |> List.int_list
  |> najdi_par 2020
  |> string_of_int

let rec najdi_trojico list = match list with
  | [] -> 0
  | x :: xs -> (
      if (najdi_par (2020 - x) xs) = 0 then najdi_trojico xs
      else x * (najdi_par (2020-x) xs)
  )

let naloga2 data = 
  let lines = List.lines data in
  lines |> List.int_list
  |> najdi_trojico 
  |> string_of_int


(* Poženemo zadevo - vzeto iz vzorcne datoteke project_windows.ms *)
let main () =
  let input_data = preberi_datoteko ("data/day_1.in") in
  let part1 = naloga1 input_data in
  let part2 = naloga2 input_data in
  print_endline part1;
  print_endline part2;
  izpisi_datoteko ("out/day_" ^ (string_of_int day) ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ (string_of_int day) ^ "_2.out") part2;
  ()

let _ = main ()