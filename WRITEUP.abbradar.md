\@vuvko (`DNIWE :: a`) and I had lots of fun this year
participating in ICFPC. This is a writeup from my, \@abbradar\'s, side,
and it focuses on the programming language part of the competition. I
was mostly active during the second part of the competition.

Because the competition server is no longer online, the task contents
have been recreated from the published source code[^1] and may be
inaccurate.

# The competition

ICFPC is a three-day annual contest focusing on open-ended tasks that
generally deal with programming languages and famous computational
problems. Teams of any size are welcome to participate, and discussions
during the contest are allowed. It is traditionally split into two
parts:

-   The lightning round, which is the first day of the competition,
    after which the first leaderboard is frozen;
-   The full contest, running for two more days with its own
    leaderboard.

After the lightning round ends, the new tasks are usually added, or the
existing ones are complicated. There\'s also always some fiction around,
and hints are given before the contest, but can usually only be
understood in hindsight[^2].

# The task

Per this year\'s legend, we established a communication channel with
space and contacted the Cult of the Bound Variable[^3], which had
started an online course on their programming language, ICFP[^4]. The
channel was an HTTP endpoint, which accepted and replied with ICFP
programs. The language itself was based on untyped lambda calculus with
a weird printable-ASCII-based encoding scheme for its terms. Apart from
the lambdas, it supported integers (of unbounded size, as we\'ll find
later), strings, and booleans. We also had a list of builtins to
implement. In the beginning, it was enough to encode and decode the
string terms.

The first interpreter was quickly written by ~~\@vuvko~~ ChatGPT in
Python. It was enough to send a first request, `get index`,
encoded `S'%4}).$%8`, and receive a welcoming message:

``` example
SB%,,/}!.$}7%,#/-%}4/}4(%}M#(//,}/&}4(%}</5.$}P!2)!",%_~~<%&/2%}4!+).'}!}#/523%j}7%}35''%34}4(!4}9/5}(!6%}!},//+}!2/5.$l}S/5e2%}./7},//+).'}!4}4(%}u).$%8wl}N/}02!#4)#%}9/52}#/--5.)#!4)/.}3+),,3j}9/5}#!.}53%}/52}u%#(/w}3%26)#%l}@524(%2-/2%j}4/}+./7}(/7}9/5}!.$}/4(%2}345$%.43}!2%}$/).'j}9/5}#!.},//+}!4}4(%}u3#/2%"/!2$wl~~;&4%2},//+).'}!2/5.$j}9/5}-!9}"%}!$-)44%$}4/}9/52}&)234}#/523%3j}3/}-!+%}352%}4/}#(%#+}4()3}0!'%}&2/-}4)-%}4/}4)-%l}C.}4(%}-%!.4)-%j})&}9/5}7!.4}4/}02!#4)#%}-/2%}!$6!.#%$}#/--5.)#!4)/.}3+),,3j}9/5}-!9}!,3/}4!+%}/52}u,!.'5!'%y4%34wl
```

Which, when decoded, became:

``` example
Hello and welcome to the School of the Bound Variable!

Before taking a course, we suggest that you have a look around. You're now looking at the [index]. To practice your communication skills, you can use our [echo] service. Furthermore, to know how you and other students are doing, you can look at the [scoreboard].

Once you are ready, please progress to one of the courses that you are currently enrolled in:

 * [lambdaman]
 * [spaceship]

After passing some tests, you may be admitted to other courses, so make sure to check this page from time to time. In the meantime, if you want to practice more advanced communication skills, you may also take our [language_test].
```

Both tasks also referenced the older ICFP contests of 2014 and 2009,
respectively, and were simplified takes on both. Apart from that, we had
a leaderboard and a language test. After running
`get leaderboard`, we found out we already got some points
for the `hello` task and more for calling the
`echo` service. The `language_test` was ignored
for now.

Each course had several different numbered tasks ---
`lambdaman` had 21, and `spaceship` 25. To remind
you, we were communicating with programs, and each task needed to be
executed to get the string with the actual assignment. Our initial
interpreter struggled with executing more than half of the
`lambdaman` tasks, and there was still the language test. So,
while \@vuvko was focusing on the tasks themselves, I, who began
actively participating closer to the end of the Lightning round, went to
work on the interpreter.

## Lightning round

The first thing was to modify the interpreter and the utils around it so
that they all consumed stdin and output to stdout. This allowed to
easily mix and match Python, Bash, and later Haskell to try out new
stuff. I then got to implement variables and lambda expressions, which
we missed, along with some refactoring. Copilot and ChatGPT both greatly
assist with these tasks. In half an hour, I had an interpreter which was
able to decode most of the `lambdaman` tasks. It was also
modular, so we could quickly add support for any new AST nodes. This was
done because I had a hunch that after the Lightning round ends, the
language might be updated, and we\'ll need to quickly modify its parts;
this didn\'t hold in the end.

Unfortunately, I quickly hit the limits of a naive implementation due to
stack overflows; the language, obviously, implemented loops via deep
recursion. While this could have been fixable in Python by implementing
thunks, I was eager to remember Haskell after several years of mostly
skipping ICFPC, and this presented an excuse.

Fast-forward 2.5 hours, and the new interpreter was ready. My mistake
was trying to do this after not touching Haskell for quite a long time.
So even though I mostly oriented around, I needed to remember all the
little things like parsing/pretty-printing libraries. I also kept the
modular design, expecting it to help later, but in the end, it only made
the code look cleaner. Having said that, I had great fun remembering how
to use Haskell for the PL implementations.

I also maintained the beta reductions counter and an optimization to
cache lambda argument computations but simulate the proper count.

The new implementation allowed us to decode all the remaining tasks.
After that I needed to leave, and when I returned, the lightning round
had already ended. I checked the language spec, but noticed no changes.
Two new tasks have been published: `3d` and
`efficiency`. `efficiency`\'s description
immediately caught my attention: the task was to evaluate 13
expressions, getting points for the answers. It implied either
evaluation optimizations or analyzing the expressions. By the time I saw
the tasks, some teams had already solved all the challenges.

This is the decoded description of the task. I didn\'t pay enough
attention to the description, and it turned out to be a *critical*
mistake later:

```` example
Welcome to the efficiency course!

In this course, your ICFP knowledge will be put to the test. To communicate with the rest of the solar system, the Cult uses their Macroware Insight evaluator. However, the School of the Bound Variable also possesses a quantum computer, on which more complex expressions can be evaluated. As the energy consumption of this quantum computer is getting out of hand, students must learn to find the evaluation result of such complex expressions with other means.

# Tip of the day

Did you know that the binary call-by-name application operator `$` has two siblings? The binary operator `~` (lazy application) is a call-by-need variant on the `$` operator, and the binary operator `!` (strict application) is the call-by-value variant. Smart usage of these can help you save many beta reductions!

# Problems

The following assignments are available:

* [efficiency1] At least one other team solved it.
* [efficiency2] At least one other team solved it.
* ...
* [efficiency13] At least one other team solved it.

Each of such ICFPs will eventually evaluate to an integer, which is the answer to the given assignment. However, students may need more than just an efficient evaluator in order to get the right answer (and likely don't possess a quantum computer). To submit an answer to a test, send:

```
solve efficiencyX answer
```

e.g. if the answer to the second assignment is `42`, then send `solve efficiency2 42`.
````

## Full round

`efficiency1` worked immediately. The next thing was to make
the interpreter pass the `language_test`, which now returned
`unary # is not correct`. After spending time to fix all the
issues I got:

``` example
Self-check OK, send `solve language_test 4w3s0m3` to claim points for it
```

It gave us the remaining points for the `hello` task. I also
took some time to implement binary `~` and `!`.
Because the optimized `$` is effectively `~`, but
without the beta reductions count trick, it was easy to do.

`efficiency2` took some glancing at, but in the Haskell AST
representation, I noticed a multiplication of the whole slow-to-compute
expression by zero and extracted the answer.

`efficiency3` is `efficiency2` but without the
zero shortcut. I couldn\'t, at first, pinpoint what happened there. The
next idea was to turn an interpreter into a translator and retry
evaluating plus adding optimizations, but I expected the organs to
thwart compilation into typed languages. Two languages came to my mind
that are both dynamic and have performant JITs: JavaScript and Lua.

I quickly wrote a naive translator to JavaScript and ran the language
test; it succeeded but returned a different string. Checking the code it
appeared that it relied on 64-bit integers, which JavaScript doesn\'t
fully support. I decided against `BigInteger` as I expected
computationally heavy expressions. Because vanilla LuaJIT also doesn\'t
support 64-bit integers, I decided to try out Julia instead, which is
dynamic, has JIT, and supports the full range of integer types.

It took some time to remember Julia and implement the lazy evaluation.
It worked, though, and passed the language test. It crashed with a stack
overflow on the application-heavy expressions, but I planned to fix that
later by introducing proper thunks.

However, before that I decided to implement a convenient debugging
output for readability. This is the self-test code in the custom syntax:

``` example
? (B= ($ ($ ($ ($ (L.3 (L.3 (L.3 (L.2 v3)))) 1) 2) 3) 4) 3) (? (B= ($ (L.3
v3) 10) 10) (? (B= (BD 3 "test") "t") (? (B= (BT 3 "test") "tes") (? (B= (B.
"te" "st") "test") (? (U! (B& True False)) (? (B& True True) (? (U! (B| False
False)) (? (B| False True) (? (B< (U- 3) (U- 2)) (? (B> 3 2) (? (B= (U- 1) (B%
(U- 3) 2)) (? (B= 1 (B% 7 3)) (? (B= (U- 1) (B/ (U- 3) 2)) (? (B= 2 (B/ 7 3)) (?
(B= 6 (B* 2 3)) (? (B= 3 (B+ 1 2)) (? (B= (U$ 15818151) "test") (? (B= (U#
"test") 15818151) (? (U! False) (? (B= (U- 3) (B- 2 5)) (? (B= 3 (B- 5 2)) (?
(B= "test" "test") (? (B= False False) (? (B= 3 3) (? True (B. (B. "Self-check
OK, send `solve language_test " (U$ (B+ 2 (B* 311 124753942619)))) "` to claim
points for it") "if is not correct") "binary = is not correct") "binary = is not
correct") "binary = is not correct") "binary - is not correct") "unary - is not
correct") "unary ! is not correct") "unary # is not correct") "unary $ is not
correct") "binary + is not correct") "binary * is not correct") "binary / is not
correct") "binary / is not correct") "binary % is not correct") "binary % is not
correct") "binary > is not correct") "binary < is not correct") "binary | is not
correct") "binary | is not correct") "binary & is not correct") "binary & is not
correct") "binary . is not correct") "binary T is not correct") "binary D is not
correct") "application is not correct") "application is not correct"
```

This immediately helped **a lot**; looking at `efficiency3`
now, I could spot that it\'s a simple recursive addition. In hindsight,
`efficiency1` threw me down the wrong optimization route,
while the goal from the beginning was to sidestep the evaluation
entirely. Which is clearly written in the task description!

I figured out the next expressions; ChatGPT helped greatly deduce the
intent when fed the custom syntax. I have also implemented a parser for
the syntax to make playing with expressions easier. Eventually, I solved
4 to 6. Another thing to help was an optimizer, which rewrote the
expressions to be more readable. I introduced a `Y` binary
function (the Y combinator) and a simple rewriter that detected it; it
immediately improved the readability a lot.

I also implemented a let-float-out optimization, which moved the
applications as high as possible in the expressions. It didn\'t help
much.

`efficiency7` and `efficiency8` looked similar ---
long lists of logical checks to find some number by iteration. ChatGPT
quickly pointed out that the checks were on the number\'s bits. It was
clearly a SAT task, so I turned to Z3.

I converted the logical check to an SMTLIB2 expression and got the
answer to `efficiency7`. `efficiency8` was harder
as the first answer was wrong; I needed to write a Python script to
search for all the possible solutions.

`efficiency9` to `efficiency11` were similar
expressions but with huge integers and operating on *nonary digits* of
them. I needed to move my interpreter to `Integer` from
`Int` to properly parse the expressions. I then converted the
first one again to an SMTLIB2 expression, but it didn\'t finish the
computation in any reasonable time.

`efficiency12` was a complex program to figure out. After
some time and several tries to rewrite it, I gave up on it; I was tired
at this point and didn\'t have much time left for the competition.

Finally, `efficiency13` OOMed when run. Analyzing it quickly
showed this was a huge string generator and an ineffective way of
counting its length. Evaluating just the generator part worked quickly
and the file size was my answer.

Unfortunately, I didn\'t have time for the remaining ones, so here are
the answers[^5]:

-   `efficiency9` to `efficiency11` turned out to
    be Sudoku puzzles! I didn\'t figure this out at all, in part because
    of the board being encoded as integers. Obviously, nonary digits
    were a huge hint that I missed;
-   `efficiency12` was a slow implementation of the phi
    function[^6].

# The lessons

Some of these are something I should have learned a long time ago
already, but long breaks between the competitions wash these out from my
mind:

-   Always pay **great** attention to the task descriptions and think of
    what they mean to hint at;
-   Have some practice before the competition starts to remember your
    tools;
-   Don\'t implement anything that is not needed right now, even if you
    are sure, at the moment, it will come in handy later;
-   LLMs are splendid tools for competitions like these.

In the end I had a wonderful time participating. We took our usual place
somewhere in the middle ;)

I also recommend reading the descriptions for other tasks and the
spoilers in the competition\'s repository[^7].

[^1]: <https://github.com/icfpcontest2024/icfpc2024>

[^2]: Except for one in the year 2020, I invite you to search for
    writeups on that one.

[^3]: A reference to the ICFPC 2006, the best we had, even though I
    didn\'t participate :)

[^4]: Interstellar Communication Functional Program:
    <https://icfpcontest2024.github.io/icfp.html>

[^5]: <https://github.com/icfpcontest2024/icfpc2024/blob/main/static/efficiency/config.yaml>

[^6]: <https://en.wikipedia.org/wiki/Euler%27s_totient_function>

[^7]: <https://github.com/icfpcontest2024/icfpc2024>
