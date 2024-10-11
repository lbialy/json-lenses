/*
 *    Copyright 2012-2017 Johannes Rudolph
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package spray.json
package lenses

import DefaultJsonProtocol._

import org.specs2.mutable.Specification
import spray.json.DeserializationException

// symbol literals will have to go:
import language.deprecated.symbolLiterals

class JsonLensesSpec extends Specification with SpecHelpers {

  import JsonLenses._

  val n = field("n")

  "Lenses" should {
    "access" in {
      "field" in {
        "existing" in {
          """{"n": 2}""".extract[Int]('n) must be_==(2)
        }
        "missing" in {
          """{"n": 2}""".extract[Int]('z) must throwAn[Exception]("""Expected field 'z' in '{"n":2}'""")
        }
        "wrong type" in {
          """{"n": 2}""".extract[String]('n) must throwA[RuntimeException](
            "spray.json.DeserializationException: Expected String as JsString, but got 2"
          )
        }
      }
      "optional field" in {
        """[{"b": 4}, {"c": 5}]""".extract[Int](* / 'b.?) must be_==(Seq(4))
      }
      "field of member" in {
        """{"n": {"b": 4}}""".extract[Int]("n" / "b") must be_==(4)
      }
      "element of array" in {
        "existing" in {
          """["a", "b", 2, 5, 8, 3]""".extract[Int](element(3)) must be_==(5)
        }
        "out of bounds" in {
          """["a", "b", 2, 5, 8, 3]""".extract[Int](element(38)) must throwAn[RuntimeException](
            "java.lang.IndexOutOfBoundsException: Too little elements in array: [\"a\",\"b\",2,5,8,3] size: 6 index: 38"
          )
        }
      }
      "finding an element" in {
        "in a homogenous array" in {
          "if type matches" in {
            """[18, 23, 2, 5, 8, 3]""".extract[Int](JsonLenses.find(JsonLenses.value.is[Int](_ < 4))) must beSome(2)
          }
          "if type is wrong" in {
            """[18, 23, 2, 5, 8, 3]""".extract[Int](JsonLenses.find(JsonLenses.value.is[String](_ < "unknown"))) must beNone
          }
        }
        "in an imhomogenous array" in {
          """["a", "b", 2, 5, 8, 3]""".extract[Int](JsonLenses.find(JsonLenses.value.is[Int](_ < 4))) must beSome(2)
          """["a", "b", 2, 5, 8, 3]""".extract[Int](JsonLenses.find(JsonLenses.value.is[String](_ == "unknown"))) must beNone
        }
        "nested finding" in {
          val lens = JsonLenses.find("a".is[Int](_ == 12)) / "b" / "c" / JsonLenses.find(JsonLenses.value.is[Int](_ == 5))

          "existing" in {
            """[{"a": 12, "b": {"c": [2, 5]}}, 13]""".extract[Int](lens) must beSome(5)
          }
          "missing in first find" in {
            """[{"a": 2, "b": {"c": [5]}}, 13]""".extract[Int](lens) must beNone
          }
          "missing in second find" in {
            """[{"a": 2, "b": {"c": [7]}}, 13]""".extract[Int](lens) must beNone
          }
        }
      }
      "orSingletonArray" in {
        "empty JsArray" in {
          "[]".extract[Seq[Int]](arrayOrSingletonAsArray) === Nil
        }
        "filled JsArray" in {
          "[1,2,3,4,5]".extract[Seq[Int]](arrayOrSingletonAsArray) === Seq(1, 2, 3, 4, 5)
        }
        "single int value" in {
          "5".extract[Seq[Int]](arrayOrSingletonAsArray) === Seq(5)
        }
        "single object value" in {
          """{ "a": 5 }""".extract[Seq[Map[String, Int]]](arrayOrSingletonAsArray) === Seq(Map("a" -> 5))
        }
      }
      "all elements of an array" in {
        "simple" in {
          """[18, 23, 2, 5, 8, 3]""".extract[Int](*) must be_==(Seq(18, 23, 2, 5, 8, 3))
        }
        "which is a scalar element" in {
          """{"a": [1, 2, 3, 4]}""".extract[Int]("a" / *) must be_==(Seq(1, 2, 3, 4))
        }
        "field of an array element" in {
          """[{"a": 1}, {"a": 2}]""".extract[Int](* / "a") must be_==(Seq(1, 2))
        }
        "nested" in {
          """[[1, 2], [3, 4]]""".extract[Int](* / *) must be_==(Seq(1, 2, 3, 4))
        }
        "if inner lens fails" in {
          """[{"a": 1}, {"b": 2}]""".extract[Int](* / 'a) must throwAn[Exception]("""Expected field 'a' in '{"b":2}'""")
          """[{"a": 1}, 12]""".extract[Int](* / 'a) must throwAn[Exception]("""Not a json object: 12""")
        }
        "nested if inner lens fails" in {
          """[{"a": [{"c": 2}, {"b": 3}]}]""".extract[Int](* / 'a / * / 'b) must throwAn[RuntimeException](
            """Expected field 'b' in '{"c":2}'"""
          )
        }
        "if outer is no array" in {
          """{"a": 5}""".extract[Int]((* / "a")) must throwAn[Exception]("""Not a json array: {"a":5}""")
          """{"a": 5}""".extract[Int]((* / *)) must throwAn[Exception]("""Not a json array: {"a":5}""")
        }
        "if inner is no array" in {
          """[{}, {}]""".extract[Int]((* / *)) must throwAn[Exception]("""Not a json array: {}""")
          """{"a": 5}""".extract[Int](("a" / *)) must throwAn[Exception]("""Not a json array: 5""")
        }
        "with nested big array" in {
          val entry = JsObject("key" -> JsString("123456789"), "doc_count" -> JsNumber(1))
          val array = JsArray(Vector.fill(10000)(entry))

          val lens = * / 'doc_count
          assert(array.extract[Long](lens) == Vector.fill(10000)(1))
        }
      }

      /*"filtered elements of an array" in {

      }*/
    }

    "modify" in {
      import JsonLenses._
      "set field" in {

        "existing" in {
          """{"n": 12}""" update (n ! set(23)) must be_json("""{"n": 23}""")
        }
        "missing" in {
          """{"n": {"b": 4}}""" update ("n" / "c" ! set(23)) must be_json("""{"n": {"b": 4, "c": 23}}""")
        }
        "twice" in {
          val a = field("a")
          """{"a": 5}""" update (a ! set(23) && a ! set(15)) must be_json("""{"a": 15}""")
        }
      }
      "update field" in {
        "existing" in {
          """{"n": 12}""" update (n ! modify[Int](_ + 1)) must be_json("""{"n": 13}""")
        }
        "wrong type" in {
          """{"n": 12}""" update (n ! modify[String](_ + "test")) must throwA[RuntimeException](
            "spray.json.DeserializationException: Expected String as JsString, but got 12"
          )
        }
        "missing" in {
          """{"n": 12}""" update (field("z") ! modify[Int](_ + 1)) must throwAn[Exception]("""Expected field 'z' in '{"n":12}'""")
        }
      }
      "optional field" in {
        "modify" in {
          """[{"b": 4}, {"c": 5}]""".update((* / 'b.?) ! modify[Int](_ + 12)) must be_json("""[{"b": 16}, {"c": 5}]""")
        }
        "create" in {
          """[{"b": 4}, {"c": 5}]""".update((* / 'b.?) ! set(38)) must be_json("""[{"b": 38}, {"c": 5, "b": 38}]""")
        }
        "set or update with default" in {
          """[{"b": 4}, {"c": 5}]""".update((* / 'b.?) ! setOrUpdateField(38)(1 + _)) must be_json("""[{"b": 5}, {"c": 5, "b": 38}]""")
        }

        "create nested (current behavior)" in {
          // One could think that nested `optionalField`s and `set` would create intermediate
          // objects as well. However, this is currently not possible, since
          //  1. the signature of UpdateLens.updated doesn't allow to operate on
          //     missing parents, which would be necessary to let child lenses
          //     control the creation of parents.
          //  2. the combine lens would then have to support it
          //
          // This test remains here as witness to the current behavior.

          """[{"b": {}}, {"c": 5}]""".update((* / 'b.? / 'd.?) ! set(38)) must be_json("""[{"b":{"d":38}},{"c":5}]""")
        }
        "delete some" in {
          def f(i: Int): Option[Int] =
            Some(i).filter(_ % 2 == 0)
          """[{"b": 4}, {"b": 3}]""".update((* / 'b.?) ! modifyOrDeleteField(f)) must be_json("""[{"b": 4}, {}]""")
        }
      }
      "set field of member" in {
        """{"n": {"b": 4}}""" update ("n" / "b" ! set(23)) must be_json("""{"n": {"b": 23}}""")
      }
      "update field of member" in {
        "existing" in {
          """{"n": {"b": 4}}""" update ("n" / "b" ! modify[Int](1 + _)) must be_json("""{"n": {"b": 5}}""")
        }
        "parent missing" in {
          """{"x": {"b": 4}}""" update ("n" / "b" ! modify[Int](1 + _)) must throwAn[Exception]("""Expected field 'n' in '{"x":{"b":4}}'""")
        }
      }
      "set element of array" in {
        """["a", "b", 2, 5, 8, 3]""" update (element(3) ! set(35)) must be_json("""["a", "b", 2, 35, 8, 3]""")
      }
      "orSingletonArray" in {
        "empty JsArray" in {
          "[]".update(arrayOrSingletonAsArray ! set(Seq(1, 2, 3, 4, 5))) must be_json("""[1, 2, 3, 4, 5]""")
        }
        "filled JsArray" in {
          "[1,2,3,4,5]".update(arrayOrSingletonAsArray / * ! modify[Int](_ + 1)) must be_json("""[2, 3, 4, 5, 6]""")
        }
        "single int value" in {
          "5".update(arrayOrSingletonAsArray / * ! modify[Int](_ + 1)) must be_json("""[6]""")
        }
        "single object value" in {
          """{ "a": 5 }""".update(arrayOrSingletonAsArray / * / 'a ! modify[Int](_ + 1)) must be_json("""[{ "a" : 6 }]""")
        }
      }
      "change a found element" in {
        "in a homogenuous array" in {
          "if found" in {
            """[12, 39, 2, 5, 8, 3]""" update (JsonLenses.find(JsonLenses.value.is[Int](_ < 4)) ! set("test")) must be_json(
              """[12, 39, "test", 5, 8, 3]"""
            )
          }
          "if not found" in {
            """[12, 39, 2, 5, 8, 3]""" update (JsonLenses.find(JsonLenses.value.is[Int](_ == 434)) ! set("test")) must be_json(
              """[12, 39, 2, 5, 8, 3]"""
            )
          }
        }
        "in an inhomogenuous array" in {
          """["a", "b", 2, 5, 8, 3]""" update (JsonLenses.find(JsonLenses.value.is[Int](_ < 4)) ! set("test")) must be_json(
            """["a", "b", "test", 5, 8, 3]"""
          )
        }
        "nested" in {
          val lens = JsonLenses.find("a".is[Int](_ == 12)) / "b" / "c" / JsonLenses.find(JsonLenses.value.is[Int](_ == 5))

          "existing" in {
            """[{"a": 12, "b": {"c": [2, 5]}}, 13]""" update (lens ! set(42)) must be_json("""[{"a": 12, "b": {"c": [2, 42]}}, 13]""")
          }
          "missing in first find" in {
            """[{"a": 2, "b": {"c": [5]}}, 13]""" update (lens ! set(42)) must be_json("""[{"a": 2, "b": {"c": [5]}}, 13]""")
          }
          "missing in second find" in {
            """[{"a": 2, "b": {"c": [7]}}, 13]""" update (lens ! set(42)) must be_json("""[{"a": 2, "b": {"c": [7]}}, 13]""")
          }
        }
      }
    }
  }
}
