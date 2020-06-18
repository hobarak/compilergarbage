

let makecounter = \init -> 
  let cur = init in \delta -> (cur = cur + delta; cur) in
let c = makecounter 10 in
(c 3; c 5)