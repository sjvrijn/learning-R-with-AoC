library(digest)

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

day3 <- function() {
    directions <- strsplit(scan(file="input3.txt", what=character()), "")[[1]]
    location <- c(0,0)
    visited <- c(list(location))
    for (d in directions) {
        if (d == "^") {
            location <- location + c(1,0)
        } else if (d == ">") {
            location <- location + c(0,1)
        } else if (d == "v") {
            location <- location + c(-1,0)
        } else if (d == "<") {
            location <- location + c(0,-1)
        }
        visited <- union(visited, c(list(location)))
    }
    print(length(visited))
    
    santas <- array(c(0,0,0,0), dim=c(2,2))
    visited <- c(list(c(0,0)))
    which <- 1  # to switch between 1 and 2
    for (d in directions) {
        if (d == "^") {
            santas[which,] <- santas[which,] + c(1,0)
        } else if (d == ">") {
            santas[which,] <- santas[which,] + c(0,1)
        } else if (d == "v") {
            santas[which,] <- santas[which,] + c(-1,0)
        } else if (d == "<") {
            santas[which,] <- santas[which,] + c(0,-1)
        }
        visited <- union(visited, c(list(santas[which,])))
        which <- 3-which
    }
    print(length(visited))
}

day4 <- function() {
    secret_key <- scan(file="input4.txt", what=character())

    # Test cases
    full_key <- paste("abcdef", as.character(609043), sep="")
    print(digest(full_key, algo="md5", serialize=F))
    full_key <- paste("pqrstuv", as.character(1048970), sep="")
    print(digest(full_key, algo="md5", serialize=F))

    number <- 0
    while (TRUE) {
        full_key <- paste(secret_key, as.character(number), sep="")
        hash <- digest(full_key, algo="md5", serialize=F)
        hash_chars <- strsplit(hash, "")[[1]]
        if (all(hash_chars[1:5] == c("0", "0", "0", "0", "0"))) {
            print(paste(number, full_key, hash))
            if (hash_chars[6] == "0") {
                break
            }
        }
        number <- number + 1
    }
    print(number)
}

day5 <- function() {
    # Read file
    
    # for string in file:
        # if:
        # - has 3 vowels?
        # - has double letter?
        # - does not contain naughty substring?
        # increase count
    # endfor
    
    # print count
}

day1()
day2()
day3()
day4()
day5()
