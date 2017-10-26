package com.thoughtworks.testgalaxy.mocks

import com.thoughtworks.testgalaxy.Galaxy
import org.mockito.Mockito.spy
import org.scalatest.mockito.MockitoSugar

trait TestMocks extends MockitoSugar {

  /**
    * testGalaryObj is a mock of Galaxy object
    * with dummy values initialized
    */
  val testGalaxyObj = new Galaxy
  val galaxySpy     = spy(new Galaxy)

  var alienToRomanMapMock: Map[String, String]  = Map("glob" -> "I",
                                                      "prok" -> "V",
                                                      "pish" -> "X",
                                                      "tegj" -> "L")

  var metalValuesMapMock: Map[String, Double]   = Map("Gold"   -> 14450.0,
                                                      "Silver" -> 17.0,
                                                      "Iron"   -> 195.5)

  testGalaxyObj.alienToRomanMap = alienToRomanMapMock
  testGalaxyObj.metalValuesMap  = metalValuesMapMock

  /**
    * testGalaryEmptyObj is a mock of Galaxy object
    * with empty values initialized
    */
  val testGalaxyEmptyObj = new Galaxy

  var alienToRomanMapEmptyMock: Map[String, String]  = Map[String, String]()

  var metalValuesMapEmptyMock: Map[String, Double]   = Map[String, Double]()

  testGalaxyEmptyObj.alienToRomanMap = alienToRomanMapEmptyMock
  testGalaxyEmptyObj.metalValuesMap  = metalValuesMapEmptyMock

  val testGalaxyMock = mock[Galaxy]

}