---
published: false
layout: post
title: Interview Questions
---

I recently had my first "real interview" in years, and bombed it appropriately.
While I go off and review my understanding of algorithmic complexity, I leave
you, dear reader, with the list of questions I was asked:

1. Tell me a little about yourself.

    I am beginning to be able to answer this question. I am a literal
    philosopher, a lover of knowledge. I learn by reading, by traveling, and by
    doing. No task is too small or menial if it is new to me and teaches me new
    things. Drudgery and routine are no good. I do not worship at the altar of
    productivity, that Baal of the business world, but I have learned that I
    must produce to be happy, and producing must take precedence over consuming.
    I support FOSS, Free/Open Source Software, as a present-day example of the
    bright future we can achieve with technology and science. Conversely I
    recognize the extreme risk to that future embodied in centralized power
    structures. I experience raw nature as a necessary, restorative force. I
    like chocolate.

    How much of this I should mention in a job interview is something I am
    incapable of determining. I am an honest man.

1. Describe a complex project you worked on.

    Unlike during previous interviews in which I had been asked this question, I
    actually recalled a suitable project to talk about. Unfortunately, I don't know
    that it was really all that complex, and I hadn't thought about it in so long
    that I couldn't offer a very detailed description.

    Of course, worrying about the specifics of the "complex project" is a rather
    simplistic approach to the question. What is its true purpose? For one, it's
    a test of communication skills, which further explains my struggle to answer
    it. I communicate very well with the written word (including real-time chat)
    but I know my verbal skills can be weak. Yet people often say I explain
    things very well, so maybe there's some anxiety in a business setting that
    trips up my tongue.

    Going deeper yet, I wonder if saying, "There's no complex problems, only
    large collections of simple problems," would get me positive or negative
    points. I suppose it's the answer I should use since it's the answer I think
    is truest. I'd just have to follow with an example project. In fact, I could
    use the same project I used in this interview! The new framing would turn it
    into an answer I would be proud of.

2. Implement a function that takes two strings as input, and returns true iff
   the strings form an anagram.

    Finally, the good stuff. Too bad I _completely_ whiffed it. A few hours
    after the interview, the proper answer occurred to me, and I will spare you
    from what transpired in the interview itself.

    Imperatively,

    1. Let `s1` and `s2` be two strings of length `N` and `M` respectively.
    1. If `M != N` return `false`.
    1. Let `m` be an empty map from character to integer.
    2. For each character `c` in `s1`,

        3. Let `m[c] += 1` (create if it doesn't already exist).
    4. For each character `c` in `s2`,

        4. If `m[c]` does not exist, return `false`.
        4. Let `m[c] -= 1`.
        4.  If `m[c] == 0`, remove it from the map.
    1. Return `true` if `m` is empty, `false` otherwise.

    Using a hash map will result in `O(N)` time complexity for the algorithm:
    One pass to build the map (step 3), and a second pass to empty it (step 4).

    The denotational semantics are as follows:

    By definition, two strings are anagrams iff (1) they contain the same set of
    characters and (2) the frequency that each character appears in each
    string is the same.

    A naive solution:

    2. Build a set of characters for each string.
    1. Build a functionBuild a map of characters to frequency count for each string,
       where the set of elements is equal to the set of characters
       existant in the string.
    2. Return true if the two sets are identical.

    1. Let `s1` and `s2` be two strings of length `N` and `M` respectively.
    2. Let `f` be a function that takes a string and a character and
       returns the number of times the character appears in the string.
    3. Let `C1` and `C2` be the sets of characters found in `s1` and `s2`
       respectively.
    4. Let `F1` be { (c1,f1) | f(c1, s1), c1 <- C1 }
    5. Let `F2` be { (c2,f2) | f(c2, s1), c2 <- C2 }
    6. Let X be { (c, f') | c == c1, c1 == c2, f' = f1 - f2, (c1,f1) <- F1, (c2,f2) <- F2 }
    7. 

    2. Let `m1` be a map relating characters in `s1` to the frequency
       those characters exist in `s1`.
    3. Let `m2` 

    Construct a map `m1` such that 

    3. Construct a function `shrink` that takes as input `m1` and `s2` and
       returns a new map, `m2`, 

    {% highlight haskell %}
    import qualified Data.Text as T

    isAnagram :: T.Text -> T.Text -> Bool
    isAnagram
    {% endhighlight %}
