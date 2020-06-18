let makecounter = \init -> 
  (let cur = init in \delta -> ( cur = (delta) + (print cur); print cur)) in
let c = makecounter 10  in
(c 11); (c 5)