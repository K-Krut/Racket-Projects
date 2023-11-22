#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))
(require rackunit)
(require rackunit/text-ui)
(require "chatterbot.rkt")

(define file-tests
  (test-suite
    "All tests for chatterbot"

    (test-case
      "babybot"
      (check-equal?
        (babybot '(I am babybot))
        '(I am babybot)
        "test 1")
      (check-equal?
        (babybot '(Why are you repeating after me ?))
        '(Why are you repeating after me ?)
        "test 2")
      (check-equal?
        (babybot '())
        '()
        "test 3")
      (check-equal?
        (babybot '(something...))
        '(something...)
        "test 4")
    )

    (test-case
      "stupidbot-creator"
      (check-equal?
        ((stupidbot-creator '(I am Groot)) '(who are you))
        '(I am Groot)
        "test 1")
      (check-equal?
        ((stupidbot-creator '(Boom!)) '(What have you just said?))
        '(Boom!)
        "test 2")
      (check-equal?
        ((stupidbot-creator '(Haha!)) '(stop saying the same thing !))
        '(Haha!)
        "test 3")
      (check-equal?
        ((stupidbot-creator '()) '())
        '()
        "test 4")
    )

    (test-case
      "matcherbot-creator"
      (check-equal?
        ((matcherbot-creator '(my name is)) '(my name is starlord))
        '(starlord)
        "test 1")
      (check-equal?
        ((matcherbot-creator '(my name is)) '(the names starlord))
        #f
        "test 2")
      (check-equal?
        ((matcherbot-creator '(my name is)) '(everybody thinks that my name is starlord))
        '(starlord)
        "test 3")
      (check-equal?
        ((matcherbot-creator '(some another pattern)) '(I wish you would use some another pattern))
        '()
        "test 4")
    )

    (test-case
      "substitutebot-creator"
      (check-equal?
        ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart love happy calm delicious)) '(bad ugly stupid))
        '(good pretty smart)
        "test 1")
      (check-equal?
        ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart love happy calm delicious)) '(Am i so bad ?))
        '(Am i so good ?)
        "test 2")
      (check-equal?
        ((substitutebot-creator '(Everyone nothing what) '(Nobody anything that)) '(Everyone says nothing except what I "don't" want to hear))
        '(Nobody says anything except that I "don't" want to hear)
        "test 3")
      (check-equal?
        ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '(testing))
        '(testing)
        "test 4")
      (check-equal?
        ((substitutebot-creator '(bad ugly stupid hate sad mad disgusting) '(good pretty smart lov happy calm delicious)) '())
        '()
        "test 5")
    )

    (test-case
      "switcherbot"
      (check-equal?
        (switcherbot '(you are smart but I am smarter than you))
        '(I am smart but you are smarter than me)
        "test 1")
      (check-equal?
        (switcherbot '(What is he saying to me ?))
        '(What is he saying to you ?)
        "test 2")
      (check-equal?
        (switcherbot '(I am the best !))
        '(you are the best !)
        "test 3")
      (check-equal?
        (switcherbot '(If I were you I "wouldn't" do this))
        '(If you was me you "wouldn't" do this)
        "test 4")
      (check-equal?
        (switcherbot '(Hm "..." you ?))
        '(Hm "..." me ?)
        "test 5")
    )

    (test-case
      "inquisitivebot"
      (check-equal?
        (inquisitivebot '(I am happy))
        '(you are happy ?)
        "test 1")
      (check-equal?
        (inquisitivebot '(I can see you))
        '(you can see me ?)
        "test 2")
      (check-equal?
        (inquisitivebot '(I wonder whether anybody love you))
        '(you wonder whether anybody love me ?)
        "test 3")
      (check-equal?
        (inquisitivebot '(you are cool program according to your words))
        '(I am cool program according to my words ?)
        "test 4")
    )

    (test-case
      "eliza"
      (check-equal?
        (eliza '(hello))
        '(hello there!)
        "test 1")
      (check-equal?
        (eliza '(I am tired of being bullied at school))
        '(why are you tired of being bullied at school ?)
        "test 2")
      (check-equal?
        (eliza '(how are you today ?))
        '(I can not answer your question.)
        "test 3")
      (check-equal?
        (eliza '())
        '(how can I help you ?)
        "test 4")
      (check-equal?
        (eliza '(I am))
        '(why are you ?)
        "test 5")
      (check-equal?
        (eliza '(hello everyone who can chat with chatbot !))
        '(hello there!)
        "test 6")
      (check-equal?
        (eliza '(""))
        '("")
        "test 7")
      (check-equal?
        (eliza '(you are the best !))
        '(I am the best !)
        "test 8")
    )

    (test-case
      "reactorbot-creator"
      (check-equal?
        ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(whats up Groot))
        '(I am Groot)
        "test 1")
      (check-equal?
        ((reactorbot-creator (stupidbot-creator '(I am Groot)) '(no Groot youll die why are you doing this) '(WE are Groot)) '(no Groot youll die why are you doing this))
        '(WE are Groot)
        "test 2")
      (check-equal?
        ((reactorbot-creator eliza '(You are just bot) '(YES I AM)) '(I am very stupid))
        '(why are you very stupid ?)
        "test 3")
      (check-equal?
        ((reactorbot-creator eliza '(you are just bot) '(YES I AM !)) '(you are just bot))
        '(YES I AM !)
        "test 4")
    )

    (test-case
      "replacerbot-creator"
      (check-equal?
        ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(youre pretty dumb))
        '(thats nice)
        "test 1")
      (check-equal?
        ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(I am hungry))
        '(hi hungry im dadbot)
        "test 2")
      (check-equal?
        ((replacerbot-creator (lambda (sent) (if (member? '? sent) '(I dont know) '(thats nice))) '(I am) '(hi) '(im dadbot)) '(what is your name ?))
        '(I dont know)
        "test 3")
      (check-equal?
        ((replacerbot-creator (stupidbot-creator '(some cool phrase !)) '(strange) '(can you suggest) '(because I "can't" understand)) '(can you say me strange unexpected thing))
        '(can you suggest unexpected thing because I "can't" understand)
        "test 4")
    )

    (test-case
      "exaggerate"
      (check-equal?
        ((exaggerate babybot 1) '(this soup is hot and tasty))
        '(this soup is very hot and very tasty)
        "test 1")
      (check-equal?
        ((exaggerate switcherbot 3) '(I am smart))
        '(you are very very very very very very very smart)
        "test 2")
      (check-equal?
        ((exaggerate (stupidbot-creator '(I am stupid bot)) 2) '(are you bot ?))
        '(I am very stupid bot)
        "test 3")
      (check-equal?
        ((exaggerate babybot 2) '(I like big salaries))
        '(I very very very like very very very big salaries)
        "test 4")
    )
))

(run-tests file-tests)