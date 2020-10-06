import patmat.Huffman._

val chars = "aaaabbbbbbcccccccssssuuuuu".toList
val t1 = createCodeTree(chars)

makeOrderedLeafList(times(chars))
encode(t1)("ab".toList)
decode(t1, encode(t1)("ab".toList))
("ab".toList == decode(t1, encode(t1)("ab".toList)))
("abs".toList == decode(t1, encode(t1)("abs".toList)))