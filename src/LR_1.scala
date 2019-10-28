import scala.collection.immutable.Stack
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.util.matching.Regex




object LR_1 {
	private final var allCharacters = new String()
	private final var relations = new ArrayBuffer[ (String, String, String) ]()
	private final var VN = new String()
	private final var VT = new String()
	private final var rowLength = 0
	private final var columnLength = 0
	private final val itemGroup = Map[ ArrayBuffer[ (String, String, String) ], Int ]()
	private final var LL1_G = new ArrayBuffer[ (String, String) ]()
	//private val allCandidateLetters = "αΑβΒγΓδΔεΕζΖηΗθΘιΙκΚλΛμΜνΝξΞοΟπΠρΡσΣτΤυΥφΦχΧψΨωΩ" + "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
	private val allCandidateLetters = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ"
	private final var usedCharacters = ""
	// private val LL1_G = ArrayBuffer( ("E", "TG"), ("G", "+TG|-TG"), ("G", "ε"), ("T", "FS"), ("S", "*FS|/FS"),
	//    ("S", "ε"), ("F", "(E)"), ("F", "i") )//, ("Y", "*FS|/FS"), ("Y", "+TG|-TG"), ("Y", "x"), ("Y", "M"), ("M", "i"), ("M", "ε") )
	//  test data 1:
	//  ( ("E", "TG"), ("G", "+TG|-TG"), ("G", "ε"), ("T", "FS"), ("S", "*FS|/FS"),
	//       ("S", "ε"), ("F", "(E)"), ("F", "i"), ("Y", "S"), ("Y", "Gx"), ("Y", "x"), ("Y", "M"), ("M", "i"), ("M", "ε") )
	//  test data 2:
	//         ( ("D", "*FD"), ("D", "ε"), ("T", "FD"), ("E", "TC"), ("F", "(E)"), ("F", "i"), ("C", "+TC"), ("C", "ε") )
	//  test data 3:
	//         ( ("E", "E+T|T"), ("T", "T*F|T"), ("F", "(E)|i") )
	//  stand test data:
	//         ( ("E", "TG"), ("G", "+TG|-TG"), ("G", "ε"), ("T", "FS"), ("S", "*FS|/FS"), ("S", "ε"), ("F", "(E)"), ("F", "i") )

	def main(args: Array[String]): Unit = {

		//test parseFile
		val result = parseFile("/home/hadoop001/Documents/code/Scala/LR(1)/testData/test.data")
		println( "the original language rules:" )
		for( rs <- result ) {
			println( rs._1 + "->" + rs._2 )
		}
		initiate("/home/hadoop001/Documents/code/Scala/LR(1)/testData/test.data")
		utility()
		println("**************")
		analyse("i+i*i#")

	}

	/*
	* Function name: utility
	* Function description: 辅助输出函数
	* Input parameters: 无
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 28 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 28 2019 +0800
	 */
	def utility(): Unit = {
		println( "after expanding the language rules:" )
		displayRelations()

		println("**************")
		//test FIRST
		println("FIRST:")
		val testFIRST = FIRST()
		for( ex <- testFIRST ) {
			println( "FIRST(" + ex._1 + ") = {" + ex._2.mkString(",") + "}" )
		}
		println("**************")

		var cnt4 = 0
		for( ex <- itemGroup.toList.sortBy(_._2) ) {
			println( cnt4 + ":\nI" + ex._2 + ":" )
			for (tx <- ex._1 ) {
				println( tx._1 + "->" + tx._2 + ", " + tx._3 )
			}
			println("^^^^^^^^^^^^^^^^^^^^^^^^")
			cnt4 += 1
		}

		println("**************")
		val test_createMatrix = createMatrix
		for ( i <- 0 to test_createMatrix.length - 1 ) {
			for ( j <- 0 to test_createMatrix(i).length - 1 ) {
				print( test_createMatrix(i)(j) + " " )
			}
			println()
		}
	}

	/*
	* Function name: analyse
	* Function description: 对指定的字符串进行LR(1)分析
	* Input parameters: -String（输入的指定字符串）
	* Return value: -Boolean（分析成功则返回true，否则false）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Mon Oct 28 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 28 2019 +0800
	 */
	def analyse( expression: String ): Boolean = {
		val statusStack = new mutable.Stack[String]()
		val characterStack = new mutable.Stack[String]()
		val analyseTable = createMatrix()
//		val analyseTable = ArrayBuffer(ArrayBuffer(null, "+", "*", "(", ")", "i", "#", "E", "T", "F", "A"),
//										ArrayBuffer("0", null, null, "S4", null, "S5", null, "1", "2", "3", null),
//										ArrayBuffer("1", "S6", null, null, null, null, "acc", null, null, null, null),
//										ArrayBuffer("2", "r2", "S7", null, null, null, "r2", null, null, null, null),
//										ArrayBuffer("3", "r4", "r4", null, null, null, "r4", null, null, null, null),
//										ArrayBuffer("4", null, null, "S4", null, "S11", null, "8", "9", "10", null),
//										ArrayBuffer("5", "r6", "r6", null, null, null, "r6", null, null, null, null),
//										ArrayBuffer("6", null, null, "S4", null, "S5", null, null, "12", "13", null),
//										ArrayBuffer("7", null, null, "S4", null, "S5", null, null, null, "14", null),
//										ArrayBuffer("8", "S15", null, null, "S16", null, null, null, null, null, null),
//										ArrayBuffer("9", "r2", "S17", null, "r2", null, null, null, null, null, null),
//										ArrayBuffer("10", "r4", "r4", null, "r4", null, null, null, null, null, null),
//										ArrayBuffer("11", "r6", "r6", null, "r6", null, null, null, null, null, null),
//										ArrayBuffer("12", "r1", "S18", null, null, null, "r1", null, null, null, null),
//										ArrayBuffer("13", "r4", "r4", null, null, null, "r4",null, null, null, null),
//										ArrayBuffer("14", "r3", "r3", null, null, null, "r3", null, null, null, null),
//										ArrayBuffer("15", null, null, "S4", null, "S11", null, null, "19", "20", null),
//										ArrayBuffer("16", "r5", "r5", null, null, null, "r5", null, null, null, null),
//										ArrayBuffer("17", null, null, "S4", null, "S11", null, null, null, "21", null),
//										ArrayBuffer("18", null, null, "S4", null, "S5", null, null, null, "14", null),
//										ArrayBuffer("19", "r1", "S17", null, "r1", null, null, null, null, null, null),
//										ArrayBuffer("20", "r4", "r4", null, "r4", null, null, null, null, null, null),
//										ArrayBuffer("21", "r3", "r3", null, "r3", null, null, null, null, null, null)
//		)
		var expr = expression
		var flag = false
		var repeat = true
		statusStack.push("0")
		characterStack.push("#")
		var tot = 0
		case class characterToColumn(a: String) {
			var ans = -1
			for( j <- 1 to (columnLength - 1) ) {
				if( analyseTable(0)(j) == a ) {
					ans = j
				}
			}
			//ans
		}
		while ( repeat == true ) {
			//  s = statusTop
			val statusTop = statusStack.top
			val a = expr(0)

			val aColumn = characterToColumn(a.toString).ans
			var sRow = statusTop.toInt
			if(sRow == 0 ) sRow += 1 else sRow += 1

			if( analyseTable(sRow)(aColumn)(0) == 'S' ) {
				val newStatus = analyseTable(sRow)(aColumn).drop(1)
				statusStack.push(newStatus)
				characterStack.push(a.toString)
				expr = expr.drop(1)

				println( tot + "状态栈： [" + displayStack(statusStack.reverse).mkString(",") + "]，符号栈：[" +
						displayStack(characterStack.reverse) + "]，剩余字符串：" + expr + "，动作：ACTION[" +
						statusTop + ", " + a + "]，" + "状态 " + aColumn + " 与符号 " + a + " 分别入栈")
				tot += 1
			}
			else if( analyseTable(sRow)(aColumn)(0) == 'r' ) {
				val exprLineNO =  analyseTable(sRow)(aColumn).drop(1).toInt
				val currentRelation = relations(exprLineNO)

				var popLength = 0
				if( currentRelation._3 != "א" ) {
					popLength = currentRelation._2.length + currentRelation._3.length
				}
				else {
					popLength = currentRelation._2.length
				}
				var cnt = popLength
				val  tmpCharacter = characterStack.reverse.toString.replace("Stack(", "").replace(")", "").replace(",", "").replace(" ", "") //.substring() //.substring(characterStack.length - popLength - 1, characterStack.length - 1 )
				val reduceCharacter = tmpCharacter.drop(tmpCharacter.length - popLength)
				while ( cnt >= 1 ) {
					statusStack.pop()
					characterStack.pop()
					cnt -= 1
				}
				// s' = characterTop
				val statusTop2 = statusStack.top.toInt

				var sRow2 = -1
				if( statusTop2 == 0 ) sRow2 = statusTop2 + 1 else sRow2 = statusTop2 + 1

				val A = currentRelation._1
				characterStack.push(A)
				val tmp = analyseTable(sRow2)( characterToColumn(A).ans )
				statusStack.push(tmp)

				println( tot + "状态栈： [" + displayStack(statusStack.reverse).mkString(",") + "]，符号栈：[" +
						displayStack(characterStack.reverse) + "]，剩余字符串：" + expr + "，动作：GOTO[" +
						statusTop2 + ", " + A + "]" + "，用产生式 " + A + "->" + reduceCharacter + " 进行规约")
				tot += 1
			}
			else if( analyseTable(sRow)(aColumn) == "acc" ) {
				println("succeed")
				flag = true
				repeat = false
			}
			else {
				println("error")
				println( "error in, sRow = " + sRow + ", aColumn = " + aColumn + ", analyseTable(" + sRow + ")(" + aColumn + ") = " + analyseTable(sRow)(aColumn) )
				flag = true
				repeat = false
			}
			//cnt += 1
		}
		if(flag) true else false
	}

	/*
	* Function name: createMatrix
	* Function description: 构造ACTION与GOTO分析表
	* Input parameters: 无
	* Return value: -Array[ Array[String] ]（分析表矩阵元素构成的二维数组）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 28 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 28 2019 +0800
	 */
	def createMatrix(): Array[ Array[String] ] = {
		val result = initiateMatrix()
		val localVT = VT
		val localVN = VN

		case class getColumn( ch: String ) {
			val matrix = initiateMatrix()
			var ans = -1
			for( j <- 0 to (columnLength - 1) ) {
				if( matrix(0)(j) == ch ) {
					ans = j
				}
			}
		}


		for( ex <- itemGroup ) {
			for( tx <- ex._1 ) {
				val pointPosition = tx._2.indexOf("·")
				//· 不在最右边
				//若项目[A->α·aβ] ∈ Ik，且GO(Ik, a) = Ij，a为终结符，则置ACTION[k, a]为“sj”
				if (pointPosition < tx._2.length - 1) {
					val a = tx._2( pointPosition + 1 )
					if( localVT.contains(a) == true && findItemOrder(ex._1, a.toString) != -1 ) {
						val j = findItemOrder(ex._1, a.toString)
						var tmpRow = -1
						tmpRow = ex._2 + 1
						result(tmpRow)( getColumn(a.toString).ans ) = "S" + j.toString
					}
				}
				if (pointPosition == tx._2.length - 1 ) {
					val a = tx._3
					var tmpRow = -1
					tmpRow = ex._2 + 1
					result(tmpRow)(getColumn(a).ans) = "r" + ( findRelationOrder( (tx._1,
							tx._2.replace("·", "") ) ) )
				}
				if( tx._1 == relations(0)._1 && tx._2 == relations(0)._2 + "·" && tx._3 == "#" ) {
					var tmpRow = -1
					tmpRow = ex._2 + 1
					result(tmpRow)( getColumn("#").ans ) = "acc"
				}
			}
			for( ch <- localVN ) {
				if( findItemOrder(ex._1, ch.toString) != -1 ) {
					val gotoNumber = findItemOrder(ex._1, ch.toString)
					var tmpRow = -1
					tmpRow = ex._2 + 1
					//A = ch
					result(tmpRow)( getColumn(ch.toString).ans ) = gotoNumber.toString
				}
			}
		}
		result
	}

	/*
	* Function name: findRelationOrder
	* Function description: 获取产生式的位于文法的第几行，从0开始
	* Input parameters: -(String, String)（給定的产生式）
	* Return value: -Int（給定的产生式在給定文法中的行数）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 28 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 28 2019 +0800
	 */
	def findRelationOrder( expression: (String, String) ): Int ={
		var ans = -1
		var cnt = 0
		val localRelations = relations
		for( ex <- localRelations ) {
			var expr = ""
			if( ex._3 != "א" ) {
				expr = ex._1 + ex._2 + ex._3
			}
			else {
				expr = ex._1 + ex._2
			}
			if( expr.equals(expression._1 + expression._2) ) {
				ans = cnt
			}
			cnt += 1
		}
		ans
	}

	/*
	* Function name: findItemOrder
	* Function description: 获取特定项目对于指定字符在项目集族中的编号，从0开始
	* Input parameters: -ArrayBuffer[ (String, String, String) ]（給定项目）, -String（給定字符）
	* Return value: -Int（給定的项目对于指定字符在项目集族中的编号）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 28 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 28 2019 +0800
	 */
	def findItemOrder( item: ArrayBuffer[ (String, String, String) ], a: String ): Int = {
		var ans = -1
		val givenItem = go( item, a).sorted
		val localItemGroup = itemGroup
		for( ex <- localItemGroup ) {
			if( ex._1.sorted.equals(givenItem) ) {
				ans = ex._2
			}
		}
		ans
	}

	/*
	* Function name: initiateMatrix
	* Function description: 初始化分析表，即ACTION表与GOTO表
	* Input parameters: 无
	* Return value: -Array[ Array[ String] ]（已完成初始的分析表）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sun Oct 27 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sun Oct 27 2019 +0800
	 */
	def initiateMatrix(): Array[ Array[ String] ] = {
		val localVN = VN
		val localVT = VT
		val tableRowLength = rowLength
		val tableColumnLength = columnLength
		val result = Array.ofDim[String](tableRowLength, tableColumnLength)
		for( j <- 1 to localVT.length ) {
			result(0)(j) = localVT(j - 1).toString
		}
		for( j <- localVT.length + 1 to tableColumnLength - 1 ) {
			result(0)(j) = localVN(j - localVT.length - 1).toString
		}
		for( i <- 1 to ( tableRowLength - 1 ) ) {
			result(i)(0) = (i - 1).toString
		}
		for( i <- 0 to (tableRowLength - 1) ) {
			for( j <- 0 to (tableColumnLength - 1) ) {
				if( result.isEmpty != false ) {
					result(i)(j) = null
				}
			}
		}

		result
	}

	/*
	* Function name: getItemGroup
	* Function description: 对于输入的文法，建立初始化的项目集
	* Input parameters: 无
	* Return value: -Unit
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 23 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sun Oct 27 2019 +0800
	 */
	def getItemGroup(): Unit = {
		val ldx = ( relations(0)._1, "·" + relations(0)._2, "#" )
		val I0 = getClosure( ArrayBuffer(ldx) )
		val wholeCharacters = allCharacters
		var tot = 0
//		var cnt = 0
		itemGroup(I0) = tot
		var appendFlag = true
		while (appendFlag == true) {
			var originalAns = Map[ ArrayBuffer[ (String, String, String) ], Int ]()
			originalAns = itemGroup.clone()
			//为什么用I作为遍历变量不行？！
			for(item <- itemGroup.keys) {
				for (ch <- wholeCharacters) {
					val newItem = go(item, ch.toString).sorted
					if (newItem.isEmpty == false && itemGroup.contains(newItem) == false) {
						tot += 1
						itemGroup(newItem) = tot
					}
				}
			}
			if( originalAns.equals(itemGroup) == true ) {
				appendFlag = false
//				println( cnt + ", all same" )
//				cnt += 1
			}
			else {
				originalAns.clear()
				originalAns = itemGroup.clone()
//				println( cnt + ", changed" )
//				cnt += 1
			}
		}
	}

	/*
	* Function name: getItems
	* Function description: 返回文法的初始项目集I0
	* Input parameters: 无
	* Return value: -ArrayBuffer[String]（文法的项目集，第一个元素是文法产生式左边符号，第二个是对应的右边字符串所生成的项目）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 23 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sar Oct 26 2019 +0800
	 */
	def getItems(): ArrayBuffer[ (String, String, String) ] = {
		val result = new ArrayBuffer[ (String, String, String) ]()
		val localRelations = relations
		//initiate
		for (ex <- localRelations) {
			if (ex._3 != "א") {
				result += ((ex._1, "·" + ex._2, "#"))
				result += ((ex._1, "·" + ex._3, "#"))
			}
			else {
				result += ((ex._1, "·" + ex._2, "#"))
			}
		}
		result
	}

	/*
	* Function name: go
	* Function description: 求給定项目对于特定字符的下一状态
	* Input parameters: -ArrayBuffer[ (String, String, String) ]（給定项目）, String（特定字符）
	* Return value: -ArrayBuffer[ (String, String, String) ]（給定项目对于特定字符的下一状态）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 26 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 26 2019 +0800
	 */
	def go( I: ArrayBuffer[ (String, String, String) ], X: String ): ArrayBuffer[ (String, String, String) ] = {
		//GO(I, X) = CLOSURE(J)
		//J = {任何形如[A->αX·β, a]的项目|[A->α·Xβ, a]∈I}
		val ans = new ArrayBuffer[ (String, String, String) ]()
		val items = new ArrayBuffer[ (String, String, String) ]()

		for( ex <- I ) {
			val pointPosition = ex._2.indexOf("·")
			//· 不在最右边
			if (pointPosition < ex._2.length - 1) {
				val A = ex._1
				val possibleX = ex._2( pointPosition + 1)
				//  αXβ
				val noPointExpressionPart2 = ex._2.replace("·", "")
				if( X == possibleX.toString ) {
					//  αX·β
					val newPart2 = noPointExpressionPart2.substring(0, pointPosition + 1) + "·" +
							noPointExpressionPart2.substring(pointPosition + 1, noPointExpressionPart2.length)
					val a = ex._3
					items += ( (A, newPart2, a) )
				}
			}
		}
		ans.appendAll( getClosure(items) )
		ans
	}

	/*
	* Function name: getClosure
	* Function description: 求給定项目集的闭包
	* Input parameters: -ArrayBuffer[ (String, String, String) ]（給定的项目集）
	* Return value: -ArrayBuffer[ (String, String, String) ]（給定项目集的闭包）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 26 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sun Oct 27 2019 +0800
	 */
	def getClosure( items: ArrayBuffer[ (String, String, String) ] ): ArrayBuffer[ (String, String, String) ] = {
		val result = new ArrayBuffer[ (String, String, String) ]()
		result.appendAll(items)
		val localFIRST = FIRST()
		var addFlag = true
		var cnt = 1
		while (addFlag == true ) {
			val originalResult = new ArrayBuffer[(String, String, String)]()
			originalResult.appendAll(result)
			for (ex <- result) {

				val pointPosition = ex._2.indexOf("·")
				//· 不在最右边
				if (pointPosition < ex._2.length - 1) {
					//B在 · 的右边
					val B = ex._2(pointPosition + 1)
					val a = ex._3

					// case 1: β != Φ and a != # or
					// case 2: β != Φ and a = #
					if (pointPosition < ex._2.length - 2) {
						val β = ex._2(pointPosition + 2)
						//  ξ
						val rightExpressionsOfB = getRightExpressions(B.toString)
						val FIRST_Of_βa = localFIRST(β.toString)
						for (b <- FIRST_Of_βa) {
							for (ksi <- rightExpressionsOfB) {
								val tmp = ((B.toString, "·" + ksi, b.toString))

								if (result.contains(tmp) == false) {
									result += tmp
								}
							}
						}
					}
					// case 3: β = Φ and a equals any character
					if (pointPosition == ex._2.length - 2) {
						val rightExpressionsOfB = getRightExpressions(B.toString)
						val FIRST_Of_βa = localFIRST(a.toString)
						for (b <- FIRST_Of_βa) {
							for (ksi <- rightExpressionsOfB) {
								val tmp = ((B.toString, "·" + ksi, b.toString))
								if (result.contains(tmp) == false) {
									result += tmp
								}
							}
						}
					}
				}
			}
			if (result != originalResult) {
				originalResult.remove(0, originalResult.length)
				originalResult.appendAll(result)
				cnt += 1
			}
			else {
				addFlag = false
				cnt += 1
			}
		}
		result
	}

	/*
	* Function name: getRightExpressions
	* Function description: 获取給定非终结符所在产生式的右部，可能不止一个，因此返回值是String类型的ArrayBuffer数组
	* Input parameters: -String（給定的非终结符）
	* Return value: -ArrayBuffer[String]（給定非终结符所在产生式的右部）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 25 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 25 2019 +0800
	 */
	def getRightExpressions(ch: String): ArrayBuffer[String] = {
		val result = new ArrayBuffer[String]()
		val localRelations = relations
		for( ex <- localRelations ) {
			if( ex._1 == ch ) {
				if( ex._3 != "א" ) {
					result += ex._2
					result += ex._3
				}
				else {
					result += ex._2
				}
			}
		}
		result
	}

	def getSingleRelation( ch: String ): ArrayBuffer[ (String, String, String) ] = {
		val result = new ArrayBuffer[ (String, String, String) ]()
		val localRelations = relations
		for( ex<- localRelations ) {
			if(ex._1 == ch ) {
				result += ex
			}
		}
		result
	}

	/*
	* Function name: splitString
	* Function description: 从字符串的首字符的左边到尾字符的右边，依次插入点“·”；每插入一个点即将其作为元素加入待返回String类型的ArrayBuffer数组
	* Input parameters: -String（待处理的String类型的字符串）
	* Return value: -ArrayBuffer[String]（输入字符串依次添加点“·”为元素的字符串数组）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 23 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 23 2019 +0800
	 */
	def splitString( string: String ): ArrayBuffer[String] = {
		val result = new ArrayBuffer[String]()
		val stringLength = string.length
		for( i <- 0 to stringLength ) {
			result += string.substring(0, i) + "·" + string.substring(i, stringLength)
		}
		result
	}

	/*
	* Function name: displayStack
	* Function description: 输出栈的所有元素
	* Input parameters: -mutable.Stack[String]（待处理的String类型的栈）
	* Return value: -String（栈所有元素组成的字符串）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 21 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 21 2019 +0800
	 */
	def displayStack( stack: mutable.Stack[String] ): String = {
		var result = ""
		for( ex <- stack ) {
			result += ex
		}
		result
	}

	/*
	* Function name: initiate
	* Function description: 初始化全局变量
	* Input parameters: the absolute path of the language-rule source file
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sun Oct 27 2019 +0800
	 */
	def initiate( filePath: String ): Unit = {
		LL1_G = parseFile(filePath)
		allCharacters = getWholeCharacters(LL1_G)
		usedCharacters = allCharacters
		relations = getRelation(LL1_G)
		VN = getVN(allCharacters)
		VT = getVT(allCharacters)
		val leftCharacters = subString(allCandidateLetters, VN)
		//relations += ( ( leftCharacters(0).toString, (relations(0)._1), "א" ) )
		relations.insert(0, ( leftCharacters(0).toString, (relations(0)._1), "א" ) )
		VN += leftCharacters(0).toString
		usedCharacters += leftCharacters(0).toString
		allCharacters += leftCharacters(0).toString

		allCharacters += "#"
		usedCharacters += "#"
		VT += "#"
		getItemGroup
		columnLength = VN.length + VT.length + 1
		rowLength = itemGroup.size + 1
	}

	/*
	* Function name: subString
	* Function description: 获取两输入字符串的差集（要求两者均非空）
	* Input parameters: 无
	* Return value: -String（两输入字符串的差集）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def subString( usedCharacters: String, localCandidateLetters: String ): String = {
		require( usedCharacters.length != 0 && localCandidateLetters.length != 0 )
		var ans = ""
		var A = usedCharacters
		var B = localCandidateLetters
		if( A.length < B.length ) {
			val tmp = A
			A = B
			B = tmp
		}
		for( i <- 0 to (A.length - 1) ) {
			var j = 0
			while( j < B.length && B(j) != A(i) ) {
				j += 1
			}
			if( j == B.length ) {
				ans += A(i)
			}
		}
		ans
	}

	/*
	* Function name: displayRelations
	* Function description: display all he language rules
	* Input parameters: 无
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def displayRelations(): Unit = {
		for( ex <- relations ) {
			if( ex._3 != "א" ) {
				println( ex._1 + "->" + ex._2 + "|" + ex._3 )
			}
			else {
				println( ex._1 + "->" + ex._2 )
			}
		}
	}

	/*
	* Function name: parseFile
	* Function description: 解析文本文件，保存在数组中
	* Input parameters: 文本绝对路径
	* Return value: -ArrayBuffer[ ( String, String ) ]（String类型的元组ArrayBuffer数组）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 18 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 18 2019 +0800
	 */
	def parseFile( filePath: String ): ArrayBuffer[ ( String, String ) ] = {
		val result = new ArrayBuffer[ ( String, String ) ]( countLines( readFromTxtByLine(filePath) ) )
		val sourceFile = readFromTxtByLine(filePath) //filePath
		for( line <- sourceFile ) {
			val tmp = line.split( "->", 2 )
			result += ( ( tmp.head, tmp.last ) )
		}
		result
	}

	/*
	* Function name: countLines
	* Function description: 计算文本行数，用于创建接收数组时开辟相应空间
	* Input parameters: -Array[String]（文本文件数据构成的数组）
	* Return value: -Int（文本行数）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 18 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def countLines( sourceFile: Array[String] ): Int = {
		var cnt = 0
		for( line <- sourceFile ) {
			cnt += 1
		}
		cnt
	}

	/*
	* Function name: readFromTxtByLine
	* Function description: 读取文本文件
	* Input parameters: -String（文本文件绝对路径）
	* Return value: -Array[String]（文本文件构成的数组，每行数据占一个数组元素）
	* Exception: -未处理
	* Author: 来自高山
	* Created date: Fri Oct 18 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 18 2019 +0800
	 */
	def readFromTxtByLine(filePath: String): Array[String] = {
		import scala.io.Source
		val source = Source.fromFile(filePath, "UTF-8")
		//val lineIterator = source.getLines()
		//lineIterator.foreach()
		val lines = source.getLines().toArray
		source.close()
		//println(lines.size)
		lines
	}

	/*
	* Function name: getWholeCharacters
	* Function description: 获取文法的除“|”之外的所有字符
	* Input parameters: -ArrayBuffer[ (String, String) ]（由文法左右两部分字符构成一个元组的数组，筛掉“|”）
	* Return value: -String（文法的除“|”之外的所有字符）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def getWholeCharacters( string: ArrayBuffer[ (String, String) ] ): String = {
		var wholeCharacters = ""
		for( expression <- string ) {
			wholeCharacters += expression._1 + expression._2
		}
		val pattern = new Regex("\\|")
		val result = pattern replaceAllIn( wholeCharacters, "" )
		if( result.isEmpty )
			"function getWholeCharacters failed"
		else
			result.distinct
	}
	/*
	* Function name: getVN
	* Function description: 获取文法的所有非终结符（non-terminal character），默认大写字母为非终结符，使用正则表达式匹配
	* Input parameters: -String（函数getWholeCharacters传来的文法的所有字符）
	* Return value: -String（文法的所有非终结符）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def getVN( string: String ): String = {
		//match big letter:
		//^[A-Z]+$
		val pattern = new Regex("[A-Z]")//("^[A-Z]+$")
		if( (pattern findAllIn string) != null )
			(pattern findAllIn string).mkString("")
		else
			"function getVN failed"
	}

	/*
	* Function name: getVT
	* Function description: 获取文法的所有非终结符（terminal character），默认大写字母外的字符为终结符，使用正则表达式匹配
	* Input parameters: -String（函数getWholeCharacters传来的文法的所有字符）
	* Return value: -String（文法的所有终结符）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def getVT( string: String ): String = {
		val pattern1 = new Regex("[A-Z]")
		val pattern2 = new Regex("\\|")
		val firstFilter = pattern1 replaceAllIn( string, "" )
		val result = pattern2 replaceAllIn( firstFilter, "" )
		if( result.isEmpty == false )
			result
		else
			return "function getVT failed"
	}
	/*
	* Function name: getRelation
	* Function description: 获取文法每一行对应的推导关系，若文法只推出了1项（即没有符号“|”），则返回元组的第三个用希伯来字母“א”示空
	* Input parameters: -ArrayBuffer[ (String, String)（已经分割好的文法左右部分构成的数组）
	* Return value: -ArrayBuffer[ (String, String, String) ]（元组第一个元素为推导式左边符号，第二为右边第二个符号串，第三为右边（若有）第三个符号串）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def getRelation( string: ArrayBuffer[ (String, String) ] ): ArrayBuffer[ (String, String, String) ] = {
		val relation = new ArrayBuffer[ (String, String, String) ]()
		for( expression <- string ) {
			if( expression._2.contains("|") == false ) {
				relation += ( ( expression._1, expression._2, "א" ) )
			}
			else {
				val tmp = expression._2.split("\\|", 2 )
				relation += ( ( expression._1, tmp.head, tmp.last ) )
			}
		}
		relation
	}

	/*
	* Function name: findFirst
	* Function description: 获取指定字符的右边两个（可能是一个）导出字符串的首个非 ε 组成的字符串
	* Input parameters: -String（指定字符）
	* Return value: -String（指定字符的右边两个（可能是一个）导出字符串的首个非 ε 组成的字符串）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def findFirst( ch: String ): String = {

		val localRelations = relations
		var result = ""
		for( ex <- localRelations ) {
			if( ch == ex._1 ) {
				if( ex._3 != "א" ) {
					if( VT.contains( ex._2(0) ) && ex._2(0) != 'ε' ) {
						result += ex._2(0).toString
					}
					if( VT.contains( ex._3(0) ) && ex._3(0) != 'ε' ) {
						result += ex._3(0).toString
					}
				}
				else {
					if( VT.contains( ex._2(0) ) && ex._2(0) != 'ε' ) {
						result += ex._2(0).toString
					}
				}
			}
		}
		result
	}

	/*
	* Function name: judgeOnlyOneVoidSuccession
	* Function description: 判断指定字符是否可推出唯一的字符ε
	* Input parameters: -String（指定字符串）
	* Return value: -Boolean（存在则true，否则false）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def judgeOnlyOneVoidSuccession( ch: String ): Boolean = {
		val localRelations = relations
		var result = 1
		for( ex <- localRelations ) {
			if( ch == ex._1 ) {
				if( ex._3 != "א" ) {
					if( ( ex._2.length == 1 && ex._2(0) == 'ε' ) || (ex._3.length == 1 && ex._3(0) == 'ε') ) {
						result = 1
					}
					else {
						result = 0
					}
				}
				else {
					if( ( ex._2.length == 1 && ex._2(0) == 'ε' ) ) {
						result = 1
					}
					else {
						result = 0
					}
				}
			}
		}
		if( result == 1 ) true else false
	}

	/*
	* Function name: judgeCaseXY
	* Function description: 判断构造FIRST集时可能的第3种情况的（1），即若X->Y...是一个产生式且Y∈VN（省略若干描述）
	* Input parameters: -Char（指定字符，即可能满足条件的产生式的左边字符）
	* Return value: -Boolean（满足则true，否则false）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 12 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 12 2019 +0800
	 */
	def judgeCaseXY( ch: Char ): Boolean = {
		val localVN = VN
		val localRelations = relations
		var result = 0
		if( localVN.contains(ch) == true ) {
			for( ex <- localRelations ) {
				if( ex._1(0) == ch ) {
					if( localVN.contains( ex._2(0) ) || localVN.contains( ex._3(0) ) ) {
						result += 1
					}
				}
			}
		}
		if( result > 0 )
			true
		else
			false
	}

	/*
	* Function name: findCase_Y_In_XY
	* Function description: 获取构造FIRST集时可能的第3种情况的（1），即若X->Y...是一个产生式且Y∈VN（省略若干描述）时的Y
	* Input parameters: -Char（指定字符，即可能满足条件的产生式的左边字符）
	* Return value: -String（Y构成的String字符串，无则为空）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 12 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 12 2019 +0800
	 */
	def findCase_Y_In_XY( ch: Char ): String = {
		val localVN = VN
		val localRelations = relations
		var result = ""
		if( localVN.contains(ch) == true ) {
			for( ex <- localRelations ) {
				if( ex._1(0) == ch ) {
					if( ex._3 != "א" ) {
						if( localVN.contains( ex._2(0) ) == true ) {
							result += ex._2(0).toString
						}
						if( localVN.contains( ex._3(0) ) == true ) {
							result += ex._3(0).toString
						}
					}
					else {
						if( localVN.contains( ex._2(0) ) == true ) {
							result += ex._2(0).toString
						}
					}
				}
			}
		}
		result
	}

	/*
	* Function name: findCase_Y_In_nY
	* Function description: 获取构造FIRST集时可能的第3种情况的（2）时的FIRST(Yi)中所有的非ε-元素（省略描述若干字）
	* Input parameters: -Char（指定字符，即可能满足条件的产生式的左边字符）
	* Return value: -String（FIRST(Yi)中所有的非ε-元素构成的String字符串，无则为空）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 12 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 12 2019 +0800
	 */
	def findCase_Y_In_nY( ch: Char ): String = {
		val localVN = VN
		val localRelations = relations
		var result = ""
		for( ex <- localRelations ) {
			if (ex._1 == ch.toString) {
				var tmp = ""

				if (ex._3 != 'א') {
					var cnt = 0
					for (tx <- ex._2) {
						// add the element belongs to tmp
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						// otherwise, reset tmp as empty string
						else {
							tmp = ""
						}
					}
					if (cnt == ex._2.length) {
						result += tmp
					}

					// reset
					cnt = 0
					tmp = ""
					for (tx <- ex._3) {
						// add the element belongs to tmp
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						// otherwise, reset result as empty string
						else {
							tmp = ""
						}
					}
					if (cnt == ex._3.length) {
						result += tmp
					}
				}
				else {
					tmp = ""
					var cnt = 0
					for (tx <- ex._2) {
						// add the element belongs to tmp
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						// otherwise, reset tmp as empty string
						else {
							tmp = ""
						}
					}
					if (cnt == ex._2.length) {
						result += tmp
					}
				}
			}
		}
		result = result.distinct
		result
	}

	/*
	* Function name: FIRST
	* Function description: 按照教材P78左下角的算法描述实现求解指定文法FIRST集；因用的是循环迭代求解，因此代码较长
	* Input parameters: -ArrayBuffer[ (String, String) ]（产生式左右两部分分别构成元组的第1个和第2个元素）
	* Return value: -Map[ String, String ]（Map的key是非终结符，value是其FIRST元素）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 14 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def FIRST(): Map[ String, String ] = {
		val FIRST_Group = Map[ String, String ]()
		val wholeCharacters = allCharacters
		val localVT = VT
		val localVN = VN

		for( character <- wholeCharacters ) {
			// case 1
			if( localVT.contains(character) ) {
				//if there exist the original key that equals the current one
				if( FIRST_Group.contains(character.toString) == true ) {
					val tmp = character.toString + FIRST_Group(character.toString)
					FIRST_Group(character.toString) = tmp.distinct
				}
				//otherwise
				else {
					FIRST_Group(character.toString) = character.toString
				}
			}

			// case 2
			if( localVN.contains(character.toString) == true ) {
				// case 2.1
				val value = findFirst(character.toString)
				if ( value.length != 0 ) {
					if ( FIRST_Group.contains(character.toString) == true ) {
						for( ch <- value ) {
							val tmp = ch + FIRST_Group(character.toString)
							FIRST_Group(character.toString) = tmp.distinct
						}
					}
					else {
						FIRST_Group(character.toString) = value.toString
					}
				}

				// case 2.2
				if( judgeOnlyOneVoidSuccession(character.toString) == true ) {
					if ( FIRST_Group.contains(character.toString) == true ) {
						val tmp = "ε" + FIRST_Group(character.toString)
						FIRST_Group(character.toString) = tmp.distinct
					}
					else {
						FIRST_Group(character.toString) = "ε"
					}
				}
			}

			for( character <- wholeCharacters ) {
				// case 3
				// case 3.1
				if( judgeCaseXY(character) == true ) {
					val tmpReply = findCase_Y_In_XY(character)
					for( eachTmpReply <- tmpReply ) {
						if( FIRST_Group.contains(eachTmpReply.toString) == true ) {
							for (ex <- FIRST_Group(eachTmpReply.toString)) {
								if (ex != 'ε') {
									if (FIRST_Group.contains(character.toString) == true) {
										val tmp = ex.toString + FIRST_Group(character.toString)
										FIRST_Group(character.toString) = tmp.distinct
									}
									else {
										FIRST_Group(character.toString) = ex.toString
									}
								}
							}
						}
					}
				}

				// case 3.2
				if( findCase_Y_In_nY(character).length > 0 ) {
					var flag = true
					val tmpReply = findCase_Y_In_nY(character)

					for( ex <- tmpReply ) {
						if( localVN.contains(ex.toString) && FIRST_Group.contains(ex.toString) == true )  {
							if( FIRST_Group(ex.toString).contains("ε") == false ) {
								flag = false
							}
						}
						else {
							flag = false
						}
						if( flag == true ) {
							if (FIRST_Group.contains(character.toString) == true) {
								val tmp = FIRST_Group(ex.toString).replace( "ε", "" ) + FIRST_Group(character.toString)
								FIRST_Group(character.toString) = tmp.distinct
							}
							else {
								FIRST_Group(character.toString) = FIRST_Group(ex.toString).replace( "ε", "" )
							}

						}
					}
				}
				// case 3.3
				if( findCase_Y_In_nY(character).length > 0 ) {
					var flag = true
					val tmpReply = findCase_Y_In_nY(character)
					for( ex <- tmpReply ) {
						if( localVN.contains(ex.toString) && FIRST_Group.contains(ex.toString) == true )  {
							if( FIRST_Group(ex.toString).contains("ε") == false ) {
								flag = false
							}
						}
						else {
							flag = false
						}
						if( flag == true ) {

							if (FIRST_Group.contains(character.toString) == true) {
								val tmp = "ε" + FIRST_Group(character.toString)
								FIRST_Group(character.toString) = tmp.distinct
							}
							else {
								FIRST_Group(character.toString) = "ε"
							}
						}
					}
				}
			}
		}
		FIRST_Group
	}

}