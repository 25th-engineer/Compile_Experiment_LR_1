import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.util.matching.Regex
object LR_1_no_line_no_annotation {
	private final var allCharacters = new String()
	private final var relations = new ArrayBuffer[ (String, String, String) ]()
	private final var VN = new String()
	private final var VT = new String()
	private final var rowLength = 0
	private final var columnLength = 0
	private final val itemGroup = Map[ ArrayBuffer[ (String, String, String) ], Int ]()
	private final var LL1_G = new ArrayBuffer[ (String, String) ]()
	private val allCandidateLetters = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ"
	private final var usedCharacters = ""
	def main(args: Array[String]): Unit = {
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
	def utility(): Unit = {
		println( "after expanding the language rules:" )
		displayRelations()
		println("**************")
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
	def analyse( expression: String ): Boolean = {
		val statusStack = new mutable.Stack[String]()
		val characterStack = new mutable.Stack[String]()
		val analyseTable = createMatrix()
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
		}
		while ( repeat == true ) {
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
				val  tmpCharacter = characterStack.reverse.toString.replace("Stack(", "").replace(")", "").replace(",", "").replace(" ", "")
				val reduceCharacter = tmpCharacter.drop(tmpCharacter.length - popLength)
				while ( cnt >= 1 ) {
					statusStack.pop()
					characterStack.pop()
					cnt -= 1
				}
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
		}
		if(flag) true else false
	}
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
					result(tmpRow)( getColumn(ch.toString).ans ) = gotoNumber.toString
				}
			}
		}
		result
	}
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
	def getItemGroup(): Unit = {
		val ldx = ( relations(0)._1, "·" + relations(0)._2, "#" )
		val I0 = getClosure( ArrayBuffer(ldx) )
		val wholeCharacters = allCharacters
		var tot = 0
		itemGroup(I0) = tot
		var appendFlag = true
		while (appendFlag == true) {
			var originalAns = Map[ ArrayBuffer[ (String, String, String) ], Int ]()
			originalAns = itemGroup.clone()
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
			}
			else {
				originalAns.clear()
				originalAns = itemGroup.clone()
			}
		}
	}
	def go( I: ArrayBuffer[ (String, String, String) ], X: String ): ArrayBuffer[ (String, String, String) ] = {
		val ans = new ArrayBuffer[ (String, String, String) ]()
		val items = new ArrayBuffer[ (String, String, String) ]()
		for( ex <- I ) {
			val pointPosition = ex._2.indexOf("·")
			if (pointPosition < ex._2.length - 1) {
				val A = ex._1
				val possibleX = ex._2( pointPosition + 1)
				val noPointExpressionPart2 = ex._2.replace("·", "")
				if( X == possibleX.toString ) {
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
				if (pointPosition < ex._2.length - 1) {
					val B = ex._2(pointPosition + 1)
					val a = ex._3
					if (pointPosition < ex._2.length - 2) {
						val β = ex._2(pointPosition + 2)
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
	def splitString( string: String ): ArrayBuffer[String] = {
		val result = new ArrayBuffer[String]()
		val stringLength = string.length
		for( i <- 0 to stringLength ) {
			result += string.substring(0, i) + "·" + string.substring(i, stringLength)
		}
		result
	}
	def displayStack( stack: mutable.Stack[String] ): String = {
		var result = ""
		for( ex <- stack ) {
			result += ex
		}
		result
	}
	def initiate( filePath: String ): Unit = {
		LL1_G = parseFile(filePath)
		allCharacters = getWholeCharacters(LL1_G)
		usedCharacters = allCharacters
		relations = getRelation(LL1_G)
		VN = getVN(allCharacters)
		VT = getVT(allCharacters)
		val leftCharacters = subString(allCandidateLetters, VN)
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
	def parseFile( filePath: String ): ArrayBuffer[ ( String, String ) ] = {
		val result = new ArrayBuffer[ ( String, String ) ]( countLines( readFromTxtByLine(filePath) ) )
		val sourceFile = readFromTxtByLine(filePath)
		for( line <- sourceFile ) {
			val tmp = line.split( "->", 2 )
			result += ( ( tmp.head, tmp.last ) )
		}
		result
	}
	def countLines( sourceFile: Array[String] ): Int = {
		var cnt = 0
		for( line <- sourceFile ) {
			cnt += 1
		}
		cnt
	}
	def readFromTxtByLine(filePath: String): Array[String] = {
		import scala.io.Source
		val source = Source.fromFile(filePath, "UTF-8")
		val lines = source.getLines().toArray
		source.close()
		lines
	}
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
	def getVN( string: String ): String = {
		val pattern = new Regex("[A-Z]")
		if( (pattern findAllIn string) != null )
			(pattern findAllIn string).mkString("")
		else
			"function getVN failed"
	}
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
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						else {
							tmp = ""
						}
					}
					if (cnt == ex._2.length) {
						result += tmp
					}
					cnt = 0
					tmp = ""
					for (tx <- ex._3) {
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
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
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
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
	def FIRST(): Map[ String, String ] = {
		val FIRST_Group = Map[ String, String ]()
		val wholeCharacters = allCharacters
		val localVT = VT
		val localVN = VN
		for( character <- wholeCharacters ) {
			if( localVT.contains(character) ) {
				if( FIRST_Group.contains(character.toString) == true ) {
					val tmp = character.toString + FIRST_Group(character.toString)
					FIRST_Group(character.toString) = tmp.distinct
				}
				else {
					FIRST_Group(character.toString) = character.toString
				}
			}
			if( localVN.contains(character.toString) == true ) {
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