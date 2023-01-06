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
            print("basement found at character")
            print(character_position)
            basement_found <- TRUE
        }
    }
    print(floor)
}

day1()
