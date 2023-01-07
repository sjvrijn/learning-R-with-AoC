day1 <- function() {
    instructions <- scan(file="input1.txt", what=character())
    x <- strsplit(instructions, "")[[1]]
    floor <- 0
    character_position <- 0
    basement_found <- FALSE
    for (c in x) {
        character_position <- character_position + 1
        if (c == "(") {
            floor <- floor + 1
        } else {
            floor <- floor - 1
        }
        if (!basement_found & floor == -1) {
            print(c("basement found at character", character_position))
            basement_found <- TRUE
        }
    }
    print(floor)
}

day2 <- function() {
    boxes <- scan(file="input2.txt", what=character())
    wrapping_paper <- 0
    ribbon <- 0
    for (box in boxes) {
        sides <- as.vector(unlist(strsplit(box, "x")), 'integer')

        face1 <- sides[1] * sides[2]
        face2 <- sides[1] * sides[3]
        face3 <- sides[2] * sides[3]
        min_face = min(c(face1, face2, face3))
        wrapping_paper <- wrapping_paper + 2*face1 + 2*face2 + 2*face3 + min_face

        max_side = max(sides)
        wrap <- 2*(sides[1]+sides[2]+sides[3]) - 2*max_side
        bow <- sides[1]*sides[2]*sides[3]
        ribbon <- ribbon + wrap + bow
    }
    print(c(wrapping_paper,ribbon))
}

day1()
day2()
