package ch04

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class ProgrammingLanguageTest : FunSpec({
    val java = ProgrammingLanguage("Java", 1995)
    val scala = ProgrammingLanguage("Scala", 2004)
    val kotlin = ProgrammingLanguage("Kotlin", 2011)
    val languages = listOf(java, scala, kotlin)

    test("names は言語名のリストを返す") {
        names(languages) shouldBe listOf("Java", "Scala", "Kotlin")
    }

    test("filterByYearAfter は指定年より後の言語を返す") {
        filterByYearAfter(languages, 2000) shouldBe listOf(scala, kotlin)
    }

    test("sortByName は名前順にソートする") {
        sortByName(languages) shouldBe listOf(java, kotlin, scala)
    }

    test("sortByYearDescending は新しい順にソートする") {
        sortByYearDescending(languages) shouldBe listOf(kotlin, scala, java)
    }

    test("averageYear は平均年を返す") {
        averageYear(listOf(java, scala)) shouldBe 1999.5
        averageYear(emptyList()) shouldBe 0.0
    }

    test("copy は一部のプロパティだけを変えた新しいインスタンスを返す") {
        renamed(kotlin, "Kotlin 2") shouldBe ProgrammingLanguage("Kotlin 2", 2011)
        kotlin.name shouldBe "Kotlin"
    }
})
