;;========================================
;; TO DO
;;========================================
;; Ecology
;; - Check death rates - find reasonable proportion between core, edge, and matrix rates []
;; - Check predatory behaviour - find out about hunting strategies []
;; - Check predator-avoidance behaviours - find out about avoidance strategies []
;; - Check foraging behaviours - find out about foraging strategies []
;; - Check dispersal behaviours - refine dispersal to be more ecologically accurate []
;;
;; Landscape
;; - Get wells working [/]
;; - Finish creating connectivity features []
;;    - Corridor Wide [/]
;;    - Corridor Narrow [/]
;;    - Stepping Stone Large [/]
;;    - Stepping Stones Small [/]
;;    - Matrix Enhancement []
;; - Make impermeable boundaries around edges []
;;
;; Prey
;; - Get early movement sorted [/]
;; - Develop movement to account for prey density []
;; - Get drinking working [/]
;; - Develop movement for drinking [/]
;; - Fix reproduction [/]
;; - Take habitat generality into account in decision making [/]
;; - Function for prey fear of predators []
;; - Prey density dependence []
;; - Fix xy spawning []
;;
;; Predators
;; - Copy and repurpose prey code for predators [/]
;; - Add hunting mechanic [/]
;; - Refine hunting []
;; - Predators density dependence []
;;
;; Observer
;; - Get line graphs working for each patch []
;; - Get stat reporters working []
;;   - Total number of island changes and average change frequency []
;;     1) Turtles have boolean attributes named change-a and change-b (both FALSE by default), and observer has change-frequency-ticker
;;        (0 by default). Turtles born outside the islands have both attributes set to FALSE. The observer also has a change-rate attribute,
;;        where the number of changes is also recorded and divided by the number of ticks since the last change.
;;     2) Presence on one island makes the corresponding change attribute TRUE
;;     3) When turtles travel to another island, and the opposite change attribute is TRUE, then it is made false and the ticker is +1,
;;        and the other change attribute is made TRUE
;;   - Average frequency of island change []
;;   - Average mortality rate []
;;     - Observer has mortality-rate attribute (0 by default), where
;;   - Average population change rate []
;; - Get heatmap working []
;;   - Ask patches to +1 when a turtle is present each tick, then upon the model stopping show the raster grid with a heatmap pallette.



;;========================================
;; SEEKING CONNECTION
;;========================================


;;===================================
;; SETUPS
;;===================================
breed [ preys prey ]
breed [ predators predator ]
globals [ islands peak-age max-age prey-deaths predator-deaths prey-change-total predator-change-total ]



patches-own [ ;; Give patches attributes to relate to turtles.
  water-capacity ;; The capacity of the water well
  my-island
]


preys-own [
  energy ;; How much energy the turtle has.
  hungry?
  dispersal-capacity ;; The maximum number of pixels a turtle can travel in one tick.
  thirst ;; How much water the turtle has.
  thirsty?
  sex ;; Male or Female.
  habitat-sensitivity ;; Mortality modifier for patch turtle-mortality-rate.
  age ;; How many ticks the turtle has lived for.
  predpres ;; Is a predator present on a target patch? ( 0 or 1 )
  vagility ;; Dispersal capacity
  vagility-multiplier
  target-distance ;; Distance to target mate / well / patch
  last-island
]


predators-own [
  energy ;; How much energy the turtle has.
  dispersal-capacity ;; The maximum number of pixels a turtle can travel in one tick.
  thirst ;; How much water the turtle has.
  sex ;; Male or Female.
  habitat-sensitivity ;; Mortality modifier for patch turtle-mortality-rate.
  age ;; How many ticks the turtle has lived for.
  predpres ;; Is a prey present on a target patch? ( 0 or 1 )
  vagility ;; Dispersal capacity
  vagility-multiplier
  target-distance ;; Distance to target mate / well / patch
  last-island
]


to setup
  clear-all ;; Blank slate.
  setup-patches
  setup-preys ;; Execute setup-turtles function.
  setup-predators ;; Execute setup-predators function
  set prey-deaths 0
  set predator-deaths 0
  set prey-change-total 0
  set predator-change-total 0
  reset-ticks ;; Set tick counter to 0.
end


to go
  ;;;; if ticks >= 500 [ stop ] ;; Stop model once certain time limit is reached - removed.
  check-death ;; Execute check-death function.
  move-predators ;; Predators move after prey
  move-preys ;; Execute move-preys function.
  eat-preys
  eat-vegetation
  regrow-vegetation ;; Execute regrow-grass function.
  replenish-well ;; Adds water to well and checks for empty well.
  tick ;; +1 to tick counter.
end



;;===================================
;; MOVEMENT
;;===================================
to move-preys
  ask preys [
    ifelse ( energy > (turtle-birth-energy + ( 2 * prey-vagility-aged ) ) ) and ( one-of preys with [ sex != [ sex ] of myself ] in-radius prey-range-of-vision != nobody ) and ( thirst > 10 ) [ ;; CHANGE
      ;; If there's enough energy to reproduce, and there's a prey of the opposite sex nearby, and the prey isn't thirsty
      let target-mate one-of preys with [ sex != [sex] of myself ] in-radius prey-range-of-vision ;; Prey chooses a mate
      face target-mate ;; Prey faces the chosen mate
      set target-distance distance target-mate ;; Finally, prey notes the distance it must travel to get to the mate
      ][
      ifelse thirst < 10 [ ;; If in fact the prey is thirsty
        let target-well min-one-of patches with [ ( pcolor = water-color or pcolor = consumed-water-color ) ] [ distance myself ] ;; or ( pcolor = consumed-water-color )
        ;; The prey chooses the closest well
        face target-well ;; Prey faces the well
        set target-distance distance target-well ;; Prey notes the distance to the target well
        ][
        let target-patch max-one-of patches in-radius prey-range-of-vision with [ not any? preys-here ] [ prey-decision-value myself ]
        ;; The target patch is the one with the highest net decision value - see prey-decision-value procedure
        ifelse target-patch != nobody [ ;; If there's not anybody on the target patch
          face target-patch ;; Prey turns to face the target patch
          set target-distance distance target-patch ;; Prey notes the distance to the target patch
          ] [
          right random 45
          left random 45
          ]
        ]
      ]
      ;; To summarise:
      ;; First the prey checks if its mate or if a well is on its patch, and if so, it'll either move to reproduce or move to drink.
      ;; Then, the prey will check if its ready to reproduce. If it is, it'll turn to face a potential mate.
      ;; If not, it'll check if its ready to drink. If it is, it'll turn to face the closest nearby watering hole.
      ;; If not, it'll choose a patch to move to to eat. If it is, it'll turn to face the best possible food patch.
      ;; Otherwise, it'll wiggle around and try again on the next tick.
    if my-island != nobody [
      set last-island my-island ;; initialize last-island after setup
    ]
    ifelse prey-vagility-aged > round target-distance [
      forward round target-distance
      set energy energy - ( round target-distance )
      set thirst thirst - 1
    ][
      forward prey-vagility-aged
      set energy energy - prey-vagility-aged
      set thirst thirst - 1
    ]
    if my-island != nobody and last-island != nobody and last-island != my-island [
      set prey-change-total prey-change-total + 1
    ]
    reproduce-preys ;; Reproduce first if conditions are met (see reproduce-preys function)
    drink-water ;; Then drink water if on a water patch
    set vagility prey-vagility-aged
    ifelse thirst < 5
      [ set thirsty? 1 ]
      [ set thirsty? 0 ]
    ifelse energy < 5
      [ set hungry? 1 ]
      [ set hungry? 0 ]
  ]
end

to move-predators ;; Very similar to the procedure for move-preys. Differences have been commented.
  ask predators [
    ifelse ( energy > ( turtle-birth-energy + ( 2 * predator-vagility-aged ) ) ) and ( one-of predators with [ sex != [ sex ] of myself ] in-radius predator-range-of-vision != nobody ) and ( thirst > 20 ) [ ;; and (patches in-radius range-of-vision != nobody) ;;  CHANGE
      let target-mate one-of predators with [ sex != [sex] of myself ] in-radius predator-range-of-vision
      face target-mate
      set target-distance distance target-mate
      ][
      ifelse thirst < 10 [
        let target-well min-one-of patches with [ ( pcolor = water-color or pcolor = consumed-water-color ) ] [ distance myself ]
        face target-well
        set target-distance distance target-well
        ][
        let target-patch max-one-of patches in-radius predator-range-of-vision with [ any? preys-here ] [ predator-decision-value myself ]
        ;; The predator targets a patch that has a prey on it and is nearby. See predator-decision-value procedure for more information.
        ifelse target-patch != nobody [ ;; Makes sure that there is a prey on the target patch.
          face target-patch ;; Turns to face the target patch.
          set target-distance distance target-patch ;; Notes the distance towards the target patch.
          ] [
          ;; search for prey
          let target-patch-none one-of patches in-radius predator-range-of-vision
          face target-patch-none
          set target-distance distance target-patch-none
          ]
        ]
      ]
    if my-island != nobody [
      set last-island my-island ;; initialize last-island after setup
    ]
    ifelse predator-vagility-aged > round target-distance [
      forward round target-distance
      set energy energy - ( round target-distance )
      set thirst thirst - 1
    ][
      forward predator-vagility-aged
      set energy energy - predator-vagility-aged
      set thirst thirst - 1
    ]
    if my-island != nobody and last-island != nobody and last-island != my-island [
      set predator-change-total predator-change-total + 1
    ]
    reproduce-predators
    drink-water
    set vagility predator-vagility-aged
  ]
end



;;===================================
;; DECISION VALUES
;;===================================
to-report prey-decision-value [ the-prey ]
	let death-risk [ prey-habitat-specialisation ] of the-prey * ( ifelse-value ;; FIX
		is-core? [ core-mortality ]
		is-edge? [ edge-mortality ]
		[ matrix-mortality ]
		)
	let energy-value ( energy-from-vegetation / ( ifelse-value
		pcolor = core-color [ energy-from-vegetation ]
		pcolor = edge-color [ energy-from-vegetation / 0.5 ]
		[ ( energy-from-vegetation / 0.1 ) ]
		) )
	ask the-prey [
    ifelse any? predators-here
	  	[ set predpres 1 ]
	  	[ set predpres 0 ]
  ]
  let distance-cost ( [ target-distance ] of the-prey / [ vagility ] of the-prey )
  report ( ( 3 * energy-value ) - death-risk ) - ( ( [ thirsty? ] of the-prey ) *  ( [ hungry? ] of the-prey ) * ( [ predpres ] of the-prey ) ) - distance-cost
end

to-report predator-decision-value [ the-predator ]
	let death-risk [ predator-habitat-specialisation ] of the-predator * ( ifelse-value
		is-core? [ core-mortality ]
		is-edge? [ edge-mortality ]
		[ matrix-mortality ]
		)
	;;let energy-value energy-from-vegetation / ( ifelse-value
	;;	is-core? [ energy-from-vegetation ]
	;;	is-edge? [ energy-from-vegetation / 0.5 ]
	;;	[ ( energy-from-vegetation / 0.1 ) ]
	;;	)
  let distance-cost ( [ target-distance ] of the-predator / [ vagility ] of the-predator )
	report ( ( 2 ) - death-risk - distance-cost ) ;; * energy-value
end

;;===================================
;; CONSUMING
;;===================================
to eat-vegetation
  ask preys [
    if pcolor = core-color [
      set pcolor consumed-core-color
      set energy energy + energy-from-vegetation ;; Eating core vegetation increases energy by amount specified by slider.
    ]
    if pcolor = edge-color [
      set pcolor consumed-edge-color
      set energy energy + (energy-from-vegetation / 2) ;; Eating core vegetation increases energy by half of core vegetation.
    ]
    if pcolor = matrix-color [
      set energy energy + (energy-from-vegetation / 10) ;; Eating core vegetation increases energy by half of core vegetation.
    ]
  ]
end

to eat-preys
  ask predators [
    let meal one-of preys-here ;; Choose a prey on the same patch
    ifelse dynamic-probability? [ ;;and ( turtle-vagility < 5 )
      if ( turtle-vagility = 10 ) and ( meal != nobody ) and ( random 100 < ( ( 0.6 ) * 100 ) ) [ ;; 50% chance of catching a prey
        ask meal [
          die
        ] ;; The prey dies
        set prey-deaths prey-deaths + 1
        set energy energy + energy-from-prey ;; The predator gains energy
      ]
      if ( turtle-vagility = 6 ) and ( meal != nobody ) and ( random 100 < ( ( 0.5 ) * 100 ) ) [ ;; 50% chance of catching a prey
        ask meal [
          die
        ] ;; The prey dies
        set prey-deaths prey-deaths + 1
        set energy energy + energy-from-prey ;; The predator gains energy
      ]
      if ( turtle-vagility = 14 ) and ( meal != nobody ) and ( random 100 < ( ( 0.7 ) * 100 ) ) [ ;; 50% chance of catching a prey
        ask meal [
          die
        ] ;; The prey dies
        set prey-deaths prey-deaths + 1
        set energy energy + energy-from-prey ;; The predator gains energy
      ]
    ][
      ifelse ( meal != nobody ) and ( random 100 < ( catch-probability * 100 ) ) [ ;; 50% chance of catching a prey
        ask meal [
          die
        ] ;; The prey dies
        set prey-deaths prey-deaths + 1
        set energy energy + energy-from-prey ;; The predator gains energy
      ][
      set age age ;; If there's no preys here, or if the coin toss fails, "do nothing"
    ]
    ]
  ]
end


to drink-water
  ask preys [
    if (pcolor = water-color) and (thirst < 10) [ ;; If the well has water in it and the prey is thirsty...
      set thirst thirst + 40 ;; Gain 20 thirst
      ask patch-here [
       set water-capacity water-capacity - 1 ;; Well loses 1 water
      ]
    ]
  ]
  ask predators [ ;; Same procedure as above.
    if (pcolor = water-color) and (thirst < 10) [
      set thirst thirst + 40
      ask patch-here [
       set water-capacity water-capacity - 1
      ]
    ]
  ]
end



;;===================================
;; REPRODUCTION
;;===================================
to reproduce-preys
  ask preys with [ ( energy > turtle-birth-energy ) and ( thirst > 20 ) ] [ ;; If the prey has enough energy to reproduce...
    let mate one-of preys-here with [ sex != [sex] of myself ] ;; Choose a prey on the space that is of the opposite sex.
    ifelse mate != nobody [ ;; If this mate exists ...
      ask mate [
       if energy > turtle-birth-energy [ ;; And it also has enough birth energy.
        set energy energy - turtle-birth-energy ;; The mate loses the birth energy
        hatch 1 [ ;; A new prey is born.
          ifelse combine-birth-energy?
            [ set energy ( turtle-birth-energy / 2 ) ]
            [ set energy ( prey-birth-energy / 2 ) ] ;; Sets the energy of the new turtle to the birth energy requirement.
          set thirst round random-normal 30 10
          set age 0 ;; Sets the age of the new turtle to 0.
          set sex one-of [ "F" "M" ] ;; Randomly assigns sex to the new turtle.
          set last-island my-island
          ]
        ask myself [
          set energy energy - turtle-birth-energy ;; The initiating prey also loses the birth energy.
        ]
      ]
    ]
    ][
    set age age ;; If none of the above conditions were met, "do nothing".
    ]
  ]
end

to reproduce-predators ;; Same procedure as above.
  ask predators with [ ( energy > turtle-birth-energy ) and ( thirst > 20 ) ] [
    let mate one-of predators-here with [ sex != [sex] of myself ]
    ifelse mate != nobody [
      ask mate [
       if energy > turtle-birth-energy [
        set energy energy - turtle-birth-energy
        hatch 1 [
          ifelse combine-birth-energy?
            [ set energy ( turtle-birth-energy / 2 ) ]
            [ set energy ( predator-birth-energy / 2 ) ] ;; Sets the energy of the new turtle to the birth energy requirement.
          set thirst round random-normal 30 10
          set age 0
          set sex one-of [ "F" "M" ]
          ifelse ( ( count predators with [ sex = "M" ] ) > ( count predators with [ sex = "F" ] ) )
           [ set sex "F" ]
           [ set sex "M" ]
          set last-island my-island
          ]
        ask myself [
          set energy energy - turtle-birth-energy
        ]
      ]
    ]
    ][
    set age age
    ]
  ]
end



;;===================================
;; MORTALITY
;;===================================
to check-death
  ask preys [
    set age age + 1 ;; Agents age by 1 tick every tick.
    if ( energy <= 0 ) or ( thirst <= 0 ) or ( age >= ( round random-normal 100 10 ) ) [
      set prey-deaths prey-deaths + 1
      die
    ] ;; Dies due to starvation, dehydration, or old age
    if pcolor = core-color and random 100 <= ( prey-habitat-specialisation * core-mortality * 100 ) [ set prey-deaths prey-deaths + 1
      die ] ;; Dies randomly from stochastic events such as tripping or falling.
    if pcolor = matrix-color and random 100 <= ( prey-habitat-specialisation * matrix-mortality * 100 ) [ set prey-deaths prey-deaths + 1
      die ]
    if pcolor = edge-color and random 100 <= ( prey-habitat-specialisation * edge-mortality * 100 ) [ set prey-deaths prey-deaths + 1
      die ]
  ]
  ask predators [
    set age age + 1 ;; Agents age by 1 tick every tick.
    if ( energy <= 0 ) or ( thirst <= 0 ) or ( age >= ( round random-normal 100 10 ) ) [
      set predator-deaths predator-deaths + 1
      die
    ] ;; Dies due to starvation, dehydration, or old age
    if pcolor = core-color and random 100 <= ( predator-habitat-specialisation * core-mortality * 100 ) [ set predator-deaths prey-deaths + 1
      die ] ;; Core death rate
    if pcolor = matrix-color and random 100 <= ( predator-habitat-specialisation * matrix-mortality * 100 ) [ set predator-deaths prey-deaths + 1
      die ]
    if pcolor = edge-color and random 100 <= ( predator-habitat-specialisation * edge-mortality * 100 ) [ set predator-deaths prey-deaths + 1
      die ]
  ]
end



;;===================================
;; VEGETATION AND WATER REGENERATION
;;===================================
to regrow-vegetation
  ask patches [
    if pcolor = consumed-core-color [ ;; If patch is consumed core.
      if random 100 < ( vegetation-growth * 100 ) [ set pcolor core-color ] ;; Core vegetation recovers according to rate set by slider.
    ]
    if pcolor = consumed-edge-color [ ;; If patch is consumed edge.
      if random 100 < ( ( vegetation-growth * 100 ) / 2) [ set pcolor edge-color ] ;; Edge vegetation recovers at half rate of core.
    ]
  ]
end

to replenish-well
  ask patches [
    if pcolor = water-color or pcolor = consumed-water-color [
      if random 100 < ( rain-frequency * 100 ) [ set water-capacity water-capacity + (random-normal 5 2) ]
      if water-capacity <= 0 [ set pcolor consumed-water-color ]
      if water-capacity > 0 [ set pcolor water-color ]
      ]
    ]
end



;;===================================
;; SETUP PREY AND PREDATOR FUNCTIONS
;;===================================
to setup-preys
  ;;set-default-shape preys "cow"
  create-preys ( prey-number ) [
    setxy ( - world-size / 2 ) ( 0 )
    set color black
    set peak-age 30
    set max-age 100
    set age random max-age
    set sex one-of [ "F" "M" ]
    set thirst round random-normal 40 10
    ifelse combine-birth-energy?
      [ set energy turtle-birth-energy ]
      [ set energy prey-birth-energy ]
    ifelse combine-vagility?
      [ set vagility round random-normal ( turtle-vagility + 2 ) prey-vagility-sd ]
      [ set vagility round random-normal prey-vagility prey-vagility-sd ] ;; two sliders - dispersal and dispersal sd
    if vagility < 1 [
      set vagility 1
    ]
    set vagility-multiplier ( vagility / 1000 )
    move-to one-of patches with [ pcolor = core-color ]
    set last-island my-island
  ]
  create-preys ( prey-number ) [
    setxy ( world-size / 2 ) ( 0 )
    set color black
    set peak-age 30
    set max-age 100
    set age random max-age
    set sex one-of [ "F" "M" ]
    set thirst round random-normal 40 10
    ifelse combine-birth-energy?
      [ set energy turtle-birth-energy ]
      [ set energy prey-birth-energy ]
    ifelse combine-vagility?
      [ set vagility round random-normal ( turtle-vagility + 2 ) prey-vagility-sd ]
      [ set vagility round random-normal prey-vagility prey-vagility-sd ] ;; two sliders - dispersal and dispersal sd
    if vagility < 1 [
      set vagility 1
    ]
    set vagility-multiplier ( vagility / 1000 )
    move-to one-of patches with [ pcolor = core-color ]
    set last-island my-island
  ]
end

to setup-predators
  ;;set-default-shape preys "cow"
  create-predators ( predator-number ) [
    setxy ( - world-size / 2 ) ( 0 )
    set color red
    set peak-age 30
    set max-age 100
    set age random max-age
    set sex one-of [ "F" "M" ]
    set thirst round random-normal 40 10
    ifelse combine-birth-energy?
      [ set energy turtle-birth-energy ]
      [ set energy predator-birth-energy ]
    ifelse combine-vagility?
      [ set vagility round random-normal ( turtle-vagility + 2 ) predator-vagility-sd ]
      [ set vagility round random-normal predator-vagility predator-vagility-sd ] ;; two sliders - dispersal and dispersal sd
    if vagility < 1 [
      set vagility 1
    ]
    set vagility-multiplier ( vagility / 1000 )
    move-to one-of patches with [ pcolor = core-color ]
    set last-island my-island
  ]
  create-predators ( predator-number ) [
    setxy ( world-size / 2 ) ( 0 )
    set color red
    set peak-age 30
    set max-age 100
    set age random max-age
    set sex one-of [ "F" "M" ]
    set thirst round random-normal 40 10
    ifelse combine-birth-energy?
      [ set energy turtle-birth-energy ]
      [ set energy predator-birth-energy ]
    ifelse combine-vagility?
      [ set vagility round random-normal ( turtle-vagility + 2 ) predator-vagility-sd ]
      [ set vagility round random-normal predator-vagility predator-vagility-sd ] ;; two sliders - dispersal and dispersal sd
    if vagility < 1 [
      set vagility 1
    ]
    set vagility-multiplier ( vagility / 1000 )
    move-to one-of patches with [ pcolor = core-color ]
    set last-island my-island
  ]
end

to-report prey-vagility-aged
  ifelse ( max (list 1 (vagility-multiplier * ((peak-age ^ 2) - (abs(peak-age - age) ^ 2))))) < turtle-vagility
    [ report max (list
      1
      round((vagility-multiplier * ((peak-age ^ 2) - (abs(peak-age - age) ^ 2)))))
    ]
    [ report turtle-vagility ]
end

to-report predator-vagility-aged
  ifelse ( max (list 1 (vagility-multiplier * ((peak-age ^ 2) - (abs(peak-age - age) ^ 2))))) < ( turtle-vagility + 1 )
    [ report max (list
      1
      round((vagility-multiplier * ((peak-age ^ 2) - (abs(peak-age - age) ^ 2)))))
    ]
    [ report ( turtle-vagility + 1 ) ]
end

;;===================================
;; SETUP PATCHES AND ISLANDS
;;===================================
to setup-patches
  ask patches [
    set pcolor matrix-color
    set my-island nobody
  ]
  set islands map square-island (list ;; agent set of patch centers to build islands from
    patch ( - world-size / 2 ) ( 0 )
    patch ( world-size / 2 ) ( 0 )
  )
  foreach islands setup-island ;; Setup islands from coordinates in list
  ask patches [
    if pcolor = water-color [
      set water-capacity starting-water
    ]
  ]
  resize-world (- world-size) (world-size) (- world-size / 1.5) (world-size / 1.5)
  if corridor-wide [
    ask patches [
      if pxcor > ( - world-size / 2 ) and pxcor < ( world-size / 2 ) and pycor < 1 and pycor > -1 and (pcolor = matrix-color or pcolor = edge-color) [
        set pcolor core-color
      ]
      if pxcor > ( - world-size / 2 ) and pxcor < ( world-size / 2 ) and ( pycor = 1 or pycor =  -1 ) and pcolor = matrix-color [
        set pcolor edge-color
      ]
    ]
  ]
  if corridor-narrow [
    ask patches [
      if ( pxcor > ( - world-size / 2 ) ) and ( pxcor < ( world-size / 2 ) ) and pycor = 0 and pcolor = matrix-color [
        set pcolor edge-color
      ]
    ]
  ]
  if step-large [
    ask patches [
      if pxcor > -3 and pxcor < 3 and pycor > -3 and pycor < 3 [ set pcolor edge-color ]
      if pxcor > -2 and pxcor < 2 and pycor > -2 and pycor < 2 [ set pcolor core-color ]
    ]
  ]
  if steps-small [
    ask patches [
      if ( pxcor = ( - world-size / ( world-size / 2 ) ) or pxcor = ( - world-size / ( world-size / 2 ) ) - 1) and ( pycor = ( 0 ) or pycor = ( 1 ) ) [ set pcolor edge-color ]
      if ( pxcor = ( world-size / ( world-size / 2 ) ) or pxcor = ( world-size / ( world-size / 2 ) ) + 1) and ( pycor = ( -1 ) or pycor = ( 0 ) ) [ set pcolor edge-color ]
    ]
  ]
end

to setup-island [ island-patches ]
  ask island-patches [
    ifelse all? neighbors [ member? self island-patches ] [ ;; If patches are part of island, it is core, otherwise is edge
      set pcolor core-color
    ] [
      set pcolor edge-color
    ]
    set my-island island-patches
  ]
  ask patches with [ abs pxcor = ( world-size / 2 ) and abs pycor = ( 0 ) ] [
    set pcolor water-color
    set water-capacity 50
  ]
end

to-report square-island [ center-patch ]
  report patches with [
    abs (pxcor - [ pxcor ] of center-patch) <= island-size and
    abs (pycor - [ pycor ] of center-patch) <= island-size
  ]
end

to-report round-island [ center-patch ]
  report [ patches in-radius island-size ] of center-patch
end

to-report core-color
  report green
end

to-report edge-color
  report lime
end

to-report matrix-color
  report white
end

to-report consumed-core-color
  report 45
end

to-report consumed-edge-color
  report 44
end

to-report water-color
  report 95
end

to-report consumed-water-color
  report 34
end

to-report is-core?
  report pcolor = core-color or pcolor = consumed-core-color or pcolor = water-color or pcolor = consumed-water-color
end

to-report is-edge?
  report pcolor = edge-color or pcolor = consumed-edge-color
end

to-report is-matrix?
  report not (is-core? or is-edge?)
end



;;===================================
;; CONNECTIVITY FEATURES
;;===================================
to new-corridor-wide ;; Connects top two patches with a wide corridor
  ask patches [
    if pxcor > ( - world-size / 2 ) and pxcor < ( world-size / 2 ) and pycor < 1 and pycor > -1 and (pcolor = matrix-color or pcolor = edge-color) [
      set pcolor core-color
    ]
    if pxcor > ( - world-size / 2 ) and pxcor < ( world-size / 2 ) and ( pycor = 1 or pycor =  -1 ) and pcolor = matrix-color [
      set pcolor edge-color
    ]
  ]
end

to new-corridor-narrow ;; Connects top two patches with a narrow corridor
  ask patches [
      if ( pxcor > ( - world-size / 2 ) ) and ( pxcor < ( world-size / 2 ) ) and pycor = 0 and pcolor = matrix-color [
        set pcolor edge-color
      ]
    ]
end

to new-step-large
  ask patches [
      if pxcor > -3 and pxcor < 3 and pycor > -3 and pycor < 3 [ set pcolor edge-color ]
      if pxcor > -2 and pxcor < 2 and pycor > -2 and pycor < 2 [ set pcolor core-color ]
    ]
end

to new-steps-small
  ask patches [
    if (pxcor = ( - world-size / ( world-size / 2 ) ) or pxcor = ( - world-size / ( world-size / 2 ) ) - 1) and ( pycor = ( 0 ) or pycor = ( 1 ) ) [ set pcolor edge-color ]
    if (pxcor = ( world-size / ( world-size / 2 ) ) or pxcor = ( world-size / ( world-size / 2 ) ) + 1) and ( pycor = ( -1 ) or pycor = ( 0 ) ) [ set pcolor edge-color ]
  ]
end





;; Written by Benjamin Lucas (2020), with assistance from Jens Madsen and Nicolas Payette.
@#$#@#$#@
GRAPHICS-WINDOW
594
10
1529
641
-1
-1
12.71
1
10
1
1
1
0
0
0
1
-36
36
-24
24
1
1
1
ticks
30.0

BUTTON
15
30
100
63
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
114
29
204
62
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
5
175
205
325
Totals
time
totals
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"prey 1" 1.0 0 -11085214 true "" "plot count preys-on item 0 islands"
"predators 1" 1.0 0 -15575016 true "" "plot count predators-on item 0 islands"
"prey 2" 1.0 0 -2674135 true "" "plot count preys-on item 1 islands"
"predators 2" 1.0 0 -10873583 true "" "plot count predators-on item 1 islands"

MONITOR
15
120
206
165
No. Vegetation Patches
count patches with [ ( pcolor = core-color ) or ( pcolor = edge-color ) ]
17
1
11

MONITOR
115
70
205
115
No. Predators
count predators
17
1
11

SLIDER
215
300
387
333
prey-number
prey-number
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
215
340
390
373
energy-from-vegetation
energy-from-vegetation
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
215
380
387
413
prey-birth-energy
prey-birth-energy
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
215
70
387
103
vegetation-growth
vegetation-growth
0
1
0.1
0.1
1
NIL
HORIZONTAL

PLOT
5
330
205
480
Number of turtles per island
NIL
NIL
0.0
4.0
0.0
100.0
false
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "clear-plot\nforeach map [ island ->\n  sum [ count turtles-here ] of island\n] islands plot"

SLIDER
405
190
560
223
island-size
island-size
0
world-width / 4
10.0
1
1
NIL
HORIZONTAL

SLIDER
215
420
385
453
prey-range-of-vision
prey-range-of-vision
0
100
12.0
1
1
NIL
HORIZONTAL

SLIDER
215
540
390
573
prey-habitat-specialisation
prey-habitat-specialisation
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
215
110
387
143
rain-frequency
rain-frequency
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
215
460
387
493
prey-vagility
prey-vagility
1
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
215
500
387
533
prey-vagility-sd
prey-vagility-sd
0
10
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
410
15
560
33
CONNECTIONS
11
0.0
1

TEXTBOX
220
285
370
303
PREY
11
0.0
1

TEXTBOX
225
15
375
33
WORLD\n
11
0.0
1

TEXTBOX
20
15
170
33
OBSERVER
11
0.0
1

TEXTBOX
420
285
570
303
PREDATORS\n
11
0.0
1

SLIDER
215
30
387
63
world-size
world-size
0
100
36.0
4
1
NIL
HORIZONTAL

SLIDER
405
460
577
493
predator-vagility
predator-vagility
1
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
405
500
577
533
predator-vagility-sd
predator-vagility-sd
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
405
540
580
573
predator-habitat-specialisation
predator-habitat-specialisation
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
405
300
577
333
predator-number
predator-number
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
405
380
575
413
predator-birth-energy
predator-birth-energy
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
405
420
580
453
predator-range-of-vision
predator-range-of-vision
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
405
340
577
373
energy-from-prey
energy-from-prey
0
100
20.0
1
1
NIL
HORIZONTAL

SWITCH
405
30
560
63
corridor-wide
corridor-wide
1
1
-1000

SWITCH
405
70
562
103
corridor-narrow
corridor-narrow
1
1
-1000

SWITCH
405
110
560
143
step-large
step-large
0
1
-1000

SWITCH
405
150
560
183
steps-small
steps-small
1
1
-1000

BUTTON
5
490
205
523
NIL
new-corridor-wide\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
530
205
563
NIL
new-corridor-narrow\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
570
205
603
NIL
new-step-large\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
610
205
643
NIL
new-steps-small\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
15
70
102
115
No. Preys
count preys
17
1
11

SLIDER
405
230
560
263
matrix-mortality
matrix-mortality
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
215
190
387
223
core-mortality
core-mortality
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
215
230
387
263
edge-mortality
edge-mortality
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
405
605
577
638
catch-probability
catch-probability
0
1
1.0
0.05
1
NIL
HORIZONTAL

SLIDER
215
150
387
183
starting-water
starting-water
0
1000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
190
695
362
728
turtle-vagility
turtle-vagility
0
20
14.0
1
1
NIL
HORIZONTAL

SLIDER
190
735
362
768
turtle-birth-energy
turtle-birth-energy
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
190
775
365
808
turtle-specialisation
turtle-specialisation
0
1
0.1
0.1
1
NIL
HORIZONTAL

SWITCH
5
695
185
728
combine-vagility?
combine-vagility?
0
1
-1000

SWITCH
5
735
185
768
combine-birth-energy?
combine-birth-energy?
0
1
-1000

SWITCH
5
775
185
808
combine-specialisation?
combine-specialisation?
1
1
-1000

TEXTBOX
15
680
165
698
BEHAVIOUR SPACE TESTING
11
0.0
1

MONITOR
375
695
457
740
Prey deaths
prey-deaths
0
1
11

MONITOR
460
695
552
740
prey changes
prey-change-total
17
1
11

MONITOR
375
745
460
790
pred deaths
predator-deaths
17
1
11

MONITOR
460
745
555
790
pred changes
predator-change-total
17
1
11

SWITCH
215
605
390
638
dynamic-probability?
dynamic-probability?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="med specialisation, large corridor + no connection" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low specialisation, large corridor + no connection" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="med specialisation, small corridor" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="med specialisation, large step" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="med specialisation, island expansion" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="med specialisation, small step" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low specialisation, small corridor" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low specialisation, large step" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low specialisation, island expansion" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="low specialisation, small step" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high specialisation, small step" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high specialisation, large step" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high specialisation, wide corridor" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high specialisation, narrow corridor" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high specialisation, island expansion" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="high specialisation, no connection" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="350"/>
    <exitCondition>not any? predators</exitCondition>
    <metric>count preys-on item 0 islands</metric>
    <metric>count preys-on item 1 islands</metric>
    <metric>count predators-on item 0 islands</metric>
    <metric>count predators-on item 1 islands</metric>
    <metric>prey-deaths</metric>
    <metric>predator-deaths</metric>
    <metric>prey-change-total</metric>
    <metric>predator-change-total</metric>
    <enumeratedValueSet variable="matrix-mortality">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-water">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="edge-mortality">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="core-mortality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps-small">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-specialisation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-size">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-vagility?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="catch-probability">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-vegetation">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="combine-birth-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-birth-energy">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-birth-energy">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-number">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-range-of-vision">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-prey">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rain-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-habitat-specialisation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vagility-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-wide">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turtle-vagility">
      <value value="6"/>
      <value value="10"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step-large">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="corridor-narrow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="island-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-range-of-vision">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vegetation-growth">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
