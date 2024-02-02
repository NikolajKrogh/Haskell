data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYear * planetYear planet)
  where 
    earthYear = 31557600 
    planetYear Mercury = 0.2408467
    planetYear Venus = 0.61519726
    planetYear Earth = 1.0
    planetYear Mars = 1.8808158
    planetYear Jupiter = 11.862615
    planetYear Saturn = 29.447498
    planetYear Uranus = 84.016846
    planetYear Neptune = 164.79132
