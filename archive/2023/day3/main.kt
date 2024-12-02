@file:Suppress("JAVA_MODULE_DOES_NOT_EXPORT_PACKAGE")

@Suppress("UNUSED_PARAMETER")
fun main(args : Array<String>) {
    val input = generateSequence(::readLine).toList();
    if (args[0] == "1") {
        part1(input);
    } else if (args[0] == "2") {
        part2(input);
    }

}

fun part1(input: List<String>) {
    var total = 0;

    var y = 0;
    var x = 0;
    var is_linked = false;
    var number = "";
    for (kar in (input.joinToString("\n")+".").windowed(1)) {
        if (kar in arrayOf("0","1","2","3","4","5","6","7","8","9")) {
            number += kar;
            is_linked = is_linked || amILinked(input, x, y);
        } else {
            if (is_linked) {
                total += number.toInt();
            }
            is_linked = false;
            number = "";
            if (kar == "\n") {
                x = -1;
                y += 1;
            }
        }
        x += 1;
    }
    
    println(total)
}

fun amILinked(input: List<String>, x: Int, y: Int): Boolean {
    return getLinked(input, x, y).first;
}

data class P(val x: Int, val y: Int) {}

fun getLinked(input: List<String>, x: Int, y: Int): Pair<Boolean, P?> {
    val max_x = input[0].length;
    val max_y = input.size;
    val nonLinkChars = arrayOf('.', '0','1','2','3','4','5','6','7','8','9');

    val grid = arrayOf(
        P(x-1, y-1), P(x, y-1), P(x+1, y-1),
        P(x-1, y),              P(x+1, y),
        P(x-1, y+1), P(x, y+1), P(x+1, y+1),
        );

    for ((nx, ny) in grid) {
        if (nx < 0 || nx >= max_x || ny < 0 || ny >= max_y) {
            continue;
        }
        if (input[ny][nx] !in nonLinkChars) {
            return Pair(true, P(nx, ny));
        }
    }
    return Pair(false, null);
}

fun isGear(input: List<String>, p: P?): Boolean {
    if (p == null) { return false; }

    return input[p.y][p.x] == '*';
}

fun part2(input: List<String>) {
    var gearMap: HashMap<P, ArrayList<Int>> = HashMap();
    
    var y = 0;
    var x = 0;
    var gearsLinked = HashSet<P>();
    var number = "";
    for (kar in (input.joinToString("\n")+".").windowed(1)) {
        if (kar in arrayOf("0","1","2","3","4","5","6","7","8","9")) {
            number += kar;
            val linkage = getLinked(input, x, y);
            if (isGear(input, linkage.second)) {
                gearsLinked.add(linkage.second!!);
            }
        } else {
            for (pos in gearsLinked) {
                gearMap.putIfAbsent(pos, ArrayList<Int>());
                gearMap.get(pos)!!.add(number.toInt());
            }
            gearsLinked.clear();
            number = "";
            if (kar == "\n") {
                x = -1;
                y += 1;
            }
        }
        x += 1;
    }

    var total = 0;
    for (gear in gearMap.values) {
        if (gear.size == 2) {
            total += gear[0] * gear[1];
        }
    }
    
    println(total)
}
