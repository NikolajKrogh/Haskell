data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse

showAnimal :: Animal -> String
showAnimal Fly = "fly"
showAnimal Spider = "spider"
showAnimal Bird = "bird"
showAnimal Cat = "cat"
showAnimal Dog = "dog"
showAnimal Goat = "goat"
showAnimal Cow = "cow"
showAnimal Horse = "horse"

reactionAnimal :: Animal -> String 
reactionAnimal Fly = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
reactionAnimal Spider = "It wriggled and jiggled and tickled inside her.\n"
reactionAnimal Bird = "How absurd to swallow a bird!\n"
reactionAnimal Cat = "Imagine that, to swallow a cat!\n"
reactionAnimal Dog = "What a hog, to swallow a dog!\n"
reactionAnimal Goat = "Just opened her throat and swallowed a goat!\n"
reactionAnimal Cow = "I don't know how she swallowed a cow!\n"
reactionAnimal Horse = error "No reaction for horse"

swallowToCatch :: Animal -> Animal -> String
swallowToCatch hunter Spider = "She swallowed the " ++ showAnimal hunter ++ " to catch the " ++ showAnimal Spider ++ " that wriggled and jiggled and tickled inside her" ++ ".\n"
swallowToCatch hunter victim = "She swallowed the " ++ showAnimal hunter ++ " to catch the " ++ showAnimal victim ++ ".\n"

firstLine :: Animal -> String
firstLine toSwallow = "I know an old lady who swallowed a " ++ showAnimal toSwallow ++ ".\n"

lastLine :: Animal -> String
lastLine Horse = "She's dead, of course!"
lastLine _ = "I don't know why she swallowed the fly. Perhaps she'll die.\n"

animals :: [Animal]
animals = [Horse, Cow, Goat, Dog, Cat, Bird, Spider, Fly] 

onePart :: Animal -> [Animal] -> String
onePart Horse _ = firstLine Horse ++ lastLine Horse ++ "\n"
onePart Fly _ = firstLine Fly ++ lastLine Fly ++ "\n"
onePart x xs = firstLine x ++ reactionAnimal x ++ ( concat $ map (\(x,y) -> swallowToCatch x y) $ zip xs (tail xs) ) ++ lastLine x ++ "\n"

song :: String
song = concat $ reverse $ goThrough animals
 where
  goThrough [] = []
  goThrough (x:xs) = onePart x (x:xs) : goThrough xs
