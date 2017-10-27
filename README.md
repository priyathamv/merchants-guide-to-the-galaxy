# merchants-guide-to-the-galaxy

This is a Galaxy where you sell common metals and dirt not in Rupees but in some Alien code.

Buying and selling over the galaxy requires to convert numbers and units, this program is to help you in doing so.
 
The numbers used for intergalactic transactions follows similar convention to the roman numerals.

Check problem_statement.txt file for a clear understanding of what this project is about.

### An example of how the input and output looks

Test input:
```
glob is I
prok is V
pish is X
tegj is L
glob glob Silver is 34 Credits
glob prok Gold is 57800 Credits
pish pish Iron is 3910 Credits
how much is pish tegj glob glob ?
how many Credits is glob prok Silver ?
how many Credits is glob prok Gold ?
how many Credits is glob prok Iron ?
how much wood could a woodchuck chuck if a woodchuck could chuck wood ?
```
Test Output:
```
pish tegj glob glob is 42
glob prok Silver is 68 Credits
glob prok Gold is 57800 Credits
glob prok Iron is 782 Credits
I have no idea what you are talking about
Explain what these tests test and why
```

## Getting Started
```
sbt "run any_file_name.txt"
```
or just <br>
```
sbt run
```
### Prerequisites

* Java 1.7 or above
* sbt 0.13.16

### Installing sbt on CentOS

Download the SBT rpm package
```
http://dl.bintray.com/sbt/rpm/sbt-0.13.6.rpm
```
Run the yum command to install
```
sudo yum install sbt-0.13.6.rpm
```

## Running the tests

sbt test

## Built With

* [scala](http://docs.scala-lang.org/) - The language used
* [sbt](http://www.scala-sbt.org/0.13/docs/index.html) - Build tool
* [scalatest](http://www.scalatest.org/) - Testing tool

## Authors

* **Vinnakota Priyatam** - [Github](https://github.com/priyathamv)