case class Foo(a: String, b: String)

case class Baz(c: Int, f: Foo)

@main def launcher: Unit = 
    val a = Baz(2, Foo("a1", "b1"))
    val b = Baz(2, Foo("a2", "b2"))
    val h = compare(a, b).head
    println(diffToClass(h))

enum Diff:
    case ZeroDepthDiff(field: String, clazz: String, message: String)
    case MultiDepthDiff(field: String, clazz: String, diff: List[Diff])

import Diff._

/*
T should be a type of Product as case classes have that property. 
This does not retrict it to case classes only but this is the closest I could get.

The ideal solution would have been to inspect the equals method of the class and check 
the elements that are using to assert equality. But I could not do that because even with reflection, it's 
really hard to get the body of methods (you can get the method name and it's signature).

So this solution assumes that the default equals method generated for a case class is not overriden.
Which means that we compare all the 'products'.

This solution compares the the case classes upto any depth (or until you encounter stack overflow error)
*/

def compare[T <: Product](a: T, b: T): List[Diff] = 
    val l = a.productElementNames zip a.productIterator
    val r = b.productElementNames zip b.productIterator

    val result = l zip r filter {
        case ((_, p1), (_, p2)) => p1 != p2
    } map {
        case ((n, p1), (_, p2)) if p1.isInstanceOf[Product] => 
            MultiDepthDiff(n, p1.getClass.getSimpleName, compare(p1.asInstanceOf[Product], p2.asInstanceOf[Product]))
        case ((n, p1), (_, p2)) =>
            ZeroDepthDiff(n, p1.getClass.getSimpleName ,s"Left = $p1, Right = $p2")
    }
    result.toList

def spacesAndString(n: Int, s: String): String = 
    (1 to n).foldLeft("")((acc, _) => acc + " ") + s
//get the class name that is causing the 
def diffToClass(diff: Diff, width: Int = 0): String = 
    diff match 
        case ZeroDepthDiff(field, clazz, message) => 
            spacesAndString(width, s"$field:$clazz") + "\n" + spacesAndString(width + 1, message)
        case MultiDepthDiff(field, clazz, d) => 
            spacesAndString(width, s"$field:$clazz") + "\n" + diffToClass(d.head, width + 1)
