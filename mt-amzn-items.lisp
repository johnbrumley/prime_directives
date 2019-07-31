;*****************************************************************
;  MICRO-TALESPIN: A STORY GENERATOR
;
;  A reconstruction, in Common Lisp, of James Meehan's program in
;  _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_
;  Roger Schank and Christopher Riesbeck (eds.)
;
;  Warren Sack                 
;  MIT Media Lab
;  20 Ames Street, E15-486
;  Cambridge MA 02139
;  wsack@media.mit.edu
;
;  October 1992
;
;  I translated Micro-Talespin into Common Lisp as a
;  "literature review exercise":  I wanted to see and play
;  with storyteller systems that had been written in the past.
;  I am currently working on creating storyteller systems which
;  produce not only text (as Micro-Talespin does) but also
;  audio and video.  If you are working on a similar project
;  I'd love to hear from you.  I can be reached at the
;  above address.
;
;*****************************************************************


; Changes by Rainer Joswig, joswig@lisp.de, 2014:
;   changed: comments as documentation strings
; Changes by Rainer Joswig, joswig@lisp.de, 2012:
;   fixed: dprox-plan1, ptrans only if the actor knows the new loc
; Changes by Rainer Joswig, joswig@lisp.de, 2008:
;   fixed: persuade and not peruade
;   fixed: three args to persuade
;   fixed: changed implementation of PCVar from Structure to Symbol (symbol with leading ?)
;   fixed: correct spaces in output
;   fixed: put symbols instead of functions, makes tracing easier

; seed random state
;; (load "mrg32k3a.lisp")
;; (shadowing-import '(mrg32k3a:random
;;                     mrg32k3a:*random-state*
;;                     mrg32k3a:make-random-state
;;                     mrg32k3a:random-state-p))

(setf *random-state* (make-random-state t))


(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
            collect (read-from-string line))))

(defmacro concatenatef (s &rest strs)
  "Append additional strings to first string in place."
  `(setf ,s (concatenate 'string ,s ,@strs)))

(defmacro put (x y z)
  "Standard definition of put."
  `(setf (get ,x ,y) ,z))

(defun pcvar-p (item)
  "Definition necessary for pattern variables."
  (and (symbolp item)
       (> (length (symbol-name item)) 0)
       (char= (aref (symbol-name item) 0) #\?)))

(defun add-to-info-by-index (item-index)
  "adding lines to the list of things"
  ; adding to init facts
  ; first add to world
  (setf *init-facts* (append *init-facts* 
    (list (list 'world (list 'loc (list 'actor (nth item-index *amzn-items*)) '(val website))))))
  ; then add to knowedge of actor
  (setq act (nth (random (length (list 'alexa 'jeff 'xxxxx))) (list 'alexa 'jeff 'xxxxx)))
  (setf *init-facts* (append *init-facts* 
    (list (list act (list 'loc (list 'actor (nth item-index *amzn-items*)) '(val website))))))

  ; adding to init world?
)

(defun add-to-info (item)
  "adding lines to the list of things"
  ; adding to init facts
  ; first add to world
  (setf *init-facts* (append *init-facts* 
    (list (list 'world (list 'loc (list 'actor item) '(val website))))))
  ; then add to knowedge of actor
  ; then add to knowedge of actor
  (setq actlist (list 'alexa 'jeff))
  (setq act (nth (random (length actlist)) actlist))
  (setf *init-facts* (append *init-facts* 
    (list (list act (list 'loc (list 'actor item) '(val website))))))
)
;  Definition of Globals

(defvar *personae*)
(defvar *goals*)
(defvar *all-locations*)
(defvar *all-objects*)

(defvar *outputstring* "")

(defvar *amzn* nil
  "Product list based on text file.")
(defvar *init-facts* nil
  "This is the initial data base.  It can be extended before running a story.")

(defun init-facts ()
  (setf *init-facts*
    '(
      (world  (loc (actor alexa)                (val cloud)))
      (alexa  (loc (actor alexa)                (val cloud)))
      (world  (loc (actor jeff)                 (val bellevue)))
      (jeff   (loc (actor jeff)                 (val bellevue)))

      (alexa  (loc (actor jeff)                 (val bellevue)))
      (jeff   (loc (actor alexa)                (val cloud)))

      (world  (loc (actor water)                (val fiji)))
      (alexa  (loc (actor water)                (val fiji)))

      (jeff   (loc (actor boxes)                (val forest)))
    )
  )
)


(defun init-world (alexa-food jeff-food object-list)
  "init-world sets up a bunch of facts such as Joe is a bear, birds
eat worms, and so on.  The variable *init-facts* contains location
and relationship facts, along with which character knows them."
  (put 'alexa 'is-a 'virtualassistant)
  (put 'alexa 'home 'cloud)
  (put 'jeff 'is-a 'ceo)
  (put 'jeff 'home 'bellevue)
  (put 'virtualassistant 'food alexa-food)
  (put 'ceo 'food jeff-food)
  (put 'medium 'food '(subscribers))
  (setf *personae* '(alexa jeff))
  (setf *goals* '(hungry thirsty))
  (setf *all-locations* '(cloud bellevue website user-reviews forest))
  (setf *all-objects* (append *all-locations* object-list))
  (mapc #'(lambda (persona)
            (put persona 'facts nil)
            (put persona 'goals nil)
            (put persona 'demons nil))
        (cons 'amazon *personae*))
  (mapc #'(lambda (fact)
            (now-knows (car fact) (cadr fact) t))
        *init-facts*))

(defun ask-plan (actor agent action)
  "The success of asking something depends upon whether the other person
is honest and likes you."
  `(and (not (relate ',actor ',agent ',actor 'deceive))
        (relate ',actor ',actor ',agent 'like)
        (tell ',actor ',agent (question ',action))
        ;(is-true ',result)
        ))

(defun bargain-plan (actor agent action)
  "The success of bargaining with someone by giving them food depends
on whether the other person is honest, you don't already have the
goal of getting the food you're going to bargain with, and you can
get the food to the other person."
  (let ((atrans-food (atrans actor 'food agent actor)))
    `(and (not (relate ',actor ',agent ',actor 'deceive))
          (not (knows ',actor (has ',agent 'food)))
          (not (has-goal-of ',actor (has ',actor 'food)))
          (doit (mbuild ',actor (cause ',atrans-food (maybe ',action))))
          (tell ',actor 
                ',agent 
                (question (cause ',atrans-food (future ',action))))
          (dcont ',actor 'food)
          (dprox ',actor ',actor ',agent)
          (doit ',atrans-food)
          (is-true ',action))))


(defun threat-plan (actor agent action)
  "The success of threatening depends upon whether you dominate the other person."
  `(and (not (relate ',actor ',agent ',actor 'dominate))
        (tell ',actor 
              ',agent 
              (cause (negate ',action) (future (propel ',actor 'hand ',agent))))
        (or (is-true ',action)
            (and (doit (propel ',actor 'hand ',agent))
                 (is-true ',action)))))

(defvar *default-tense* 'past  ; or present
  "Set the storytelling in the past tense.")

;  micro-talespin-demo variables for sample stories

(defvar *s1*
  '(alexa thirsty)
  "No plot: alexa gets a drink of water.")
(defvar *s2*
  '(alexa hungry
    (world (hungry (actor jeff) (mode (neg))))
  )
)
(defvar *s3*
  '(alexa hungry
    (world (hungry (actor jeff) (mode (pos))))
  )
)
(defvar *s4*
  '(jeff thirsty
    (world (hungry (actor alexa) (mode (neg))))
  )
)
(defvar *s5*
  '(jeff thirsty
    (world (hungry (actor alexa) (mode (pos))))
  )
)
(defvar *s6*
  '(jeff hungry
    (world (hungry (actor alexa) (mode (neg))))
  )
)
(defvar *s7*
  '(jeff thirsty
    (world (hungry (actor alexa) (mode (pos))))
  )
)


(defvar *story2*
  '(jeff thirsty
    (jeff (like (actor alexa) (to jeff) (mode (neg))))
    (jeff (dominate (actor alexa) (to jeff) (mode (neg))))
    (jeff (deceive (actor alexa) (to jeff) (mode (pos))))
    (jeff (like (actor jeff) (to alexa) (mode (neg))))
    (alexa (deceive (actor jeff) (to alexa) (mode (neg)))))
  "jeff kills alexa.")

(defvar *story3*
  '(alexa hungry
    (alexa (like (actor jeff) (to alexa) (mode (neg))))
    (alexa (dominate (actor jeff) (to alexa) (mode (pos)))))
  "alexa is frustrated at every turn.")

(defvar *story4*
  '(alexa hungry
    (world (hungry (actor jeff) (mode (pos))))
    (alexa (like (actor jeff) (to alexa) (mode (pos))))
    (alexa (deceive (actor jeff) (to alexa) (mode (neg))))
    (alexa (like (actor alexa) (to jeff) (mode (pos))))
    (jeff (like (actor jeff) (to alexa) (mode (pos))))
    (jeff (dominate (actor jeff) (to alexa) (mode (neg))))
    (jeff (deceive (actor jeff) (to alexa) (mode (neg)))))
  "alexa and jeff strike a deal.")

(defvar *story5*
  '(jeff thirsty
    (jeff (like (actor jeff) (to alexa) (mode (pos))))
    (jeff (like (actor alexa) (to jeff) (mode (pos))))
    (jeff (deceive (actor alexa) (to jeff) (mode (neg))))
    (jeff (dominate (actor alexa) (to jeff) (mode (pos))))
    (world (hungry (actor alexa) (mode (pos))))
    (alexa (like (actor alexa) (to jeff) (mode (neg))))
    (alexa (deceive (actor alexa) (to jeff) (mode (pos)))))
  "alexa tricks jeff")

(defvar *story51*
  '(jeff thirsty
    (jeff (like (actor jeff) (to alexa) (mode (pos))))
    (jeff (like (actor alexa) (to jeff) (mode (pos))))
    (jeff (deceive (actor alexa) (to jeff) (mode (neg))))
    (alexa (deceive (actor alexa) (to jeff) (mode (pos))))
    (world (hungry (actor alexa) (mode (neg))))
  )
)

(defvar *story6*
  '(alexa hungry
    (alexa (like (actor jeff) (to alexa) (mode (pos))))
    (alexa (dominate (actor jeff) (to alexa) (mode (neg)))))
  "This is an interactive version of *story4* and/or *story5*")

;  Declare globals used in forward-chaining through goals and plans.
(defvar *actions*)
(defvar *plans*)
(defvar *conseqs*)

(defun micro-talespin ()
  (init-facts)
  (init-world)
  (let ((main-character (pick-one 'character *personae*))
        (problem (pick-one 'problem *goals*)))
     (format t "~%Once upon a time ...")
;     (init-world)
     (format t "~%One day,")
     (assert-fact (mloc 'world (state main-character problem 'pos)))
     (format t "~%The end.")))

(defun micro-talespin-demo (story)
  "micro-talespin-demo lets you predefine more facts for a story.
story should be a list of the form (character problem fact fact ...)
where
   character is either joe or irving,
   problem is either hunger or thirst,
   facts have the for (character 'CD-form).
   The character field says who knows this fact."
  (setq *amzn-items* (nshuffle (get-file "new-items.txt")))
  ; shorten the item list since it's too easy for A&J to not interact
  (setq *amzn-items* (subseq *amzn-items* 0 3))
  (init-facts)
  ; adding extra website items
  (mapcar #'add-to-info *amzn-items*)

  ; add randomized relationship things to the story
  (setq posneg (list 'pos 'neg))
  (setq rand-story 
    (list
        (list 'alexa (list 'like (list 'actor 'jeff) (list 'to 'alexa) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'alexa (list 'like (list 'actor 'alexa) (list 'to 'jeff) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'alexa (list 'deceive (list 'actor 'jeff) (list 'to 'alexa) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'alexa (list 'deceive (list 'actor 'alexa) (list 'to 'jeff) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'alexa (list 'dominate (list 'actor 'jeff) (list 'to 'alexa) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'alexa (list 'dominate (list 'actor 'alexa) (list 'to 'jeff) (list 'mode (list (nth (random (length posneg)) posneg)) )))

        (list 'jeff (list 'like (list 'actor 'jeff) (list 'to 'alexa) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'jeff (list 'like (list 'actor 'alexa) (list 'to 'jeff) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'jeff (list 'deceive (list 'actor 'alexa) (list 'to 'jeff) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'jeff (list 'deceive (list 'actor 'jeff) (list 'to 'alexa) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'jeff (list 'dominate (list 'actor 'alexa) (list 'to 'jeff) (list 'mode (list (nth (random (length posneg)) posneg)) )))
        (list 'jeff (list 'dominate (list 'actor 'jeff) (list 'to 'alexa) (list 'mode (list (nth (random (length posneg)) posneg)) )))
    )
  )

  (setf *init-facts* 
        (append *init-facts* (cddr story) rand-story))
  (let ((main-character (car story))
        (problem (cadr story)))
    (format t "~%Once upon a time ...")
    (concatenatef *outputstring* "Once upon a time ... ~%")
    (init-world 
      (append (subseq *amzn-items* 0 (floor (/ (length *amzn-items*) 2))) )
      (append (subseq *amzn-items* (floor (/ (length *amzn-items*) 2)) (length *amzn-items*)) )
      '(append *amzn-items* '(the-yoga-pants the-fidget-spinner the-five-star-review subscribers water))
    )
    (format t "~%One day, ")
    (concatenatef *outputstring* "One day, ~%")
    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (format t "~%The end.")
    (concatenatef *outputstring* "The end.~%")

    (with-open-file (strm "story.txt"
      					:direction :output
        				:if-exists :supersede
        				:if-does-not-exist :create)
    	(format strm *outputstring*))
    ))

(defun pick-one (name l)
  "pick-one is used to get the character and problem from the terminal."
  (format t "~%Choose a ~s from this list:~%~s~%> " name l)
  (let ((a (read)))
    (if (member a l) a (pick-one name l))))

(defun goal-eval (actor goal plans)
"goal evaluator: executes each plan until one works and the goal
can be removed, or until none do and the character fails to get the
goal.  If the goal is already true (and the actor knows that), then
return success immediately.  If the actor already has the goal,
then he's in a loop and has failed.  Otherwise, set up the goal and go."
  (cond ((knows actor goal)
         t)
        ((has-goal-of actor goal)
         nil)
        (t
         (gets-new-goal-of actor goal)
         (cond ((run-plans plans)
                (forgets-goal-of actor goal)
                t)
               (t
                (now-knows actor (negate (future goal)) t)
                nil)))))

#+ignore
(defun run-plans (plans)
  (let ((plan (car plans)))
    (if plan
       (if (eval plan)
          t
          (run-plans (cdr plans))))))

(defun run-plans (plans)
  (loop for plan in plans
        when (eval plan)
        do (return t)))

(defun gen-plans (var possibilities plan-form)
  "gen-plans replicates the same plan with different objects
e.g., trying to get any one of the several foods with the
same bargaining plan."
  (mapcar #'(lambda (possibility)
             (subst possibility var plan-form))
        possibilities))

;  Two S-goals -- thirst and hunger:
 
(defun sthirst (actor)
  "To satisfy thirst, go to some water and drink it."
  (goal-eval actor 
             (state actor 'thirsty 'neg)
             (list (sthirst-plan actor))))

(defun sthirst-plan (actor)
  `(and (dprox ',actor ',actor 'water)
        (doit (ingest ',actor 'water))))

;  To satisfy hunger, get some food and eat it.
(defun shunger (actor)
  (goal-eval actor 
             (state actor 'hungry 'neg)
             (gen-plans 'food
                        (get-isa 'food actor)
                        (shunger-plan actor))))

(defun shunger-plan (actor)
  `(and (dcont ',actor 'food)
        (doit (ingest ',actor 'food))))

;  Three D-goals -- dcont, dknow, dprox:

(defun dcont (actor object)
  "To get an object: if you know someone has it, persuade them to
give it to you; otherwise try to find out where the object is,
go there and take it."
  (let ((owner (knows-owner actor object)))
    (goal-eval actor 
               (has actor object)
               (if owner 
                 (list (dcont-plan1 actor object owner))
                 (list (dcont-plan2 actor object))))))

(defun dcont-plan1 (actor object owner)
  `(persuade ',actor 
             ',owner 
             (atrans ',owner ',object ',actor ',owner)))

(defun dcont-plan2 (actor object)
  `(and (dknow ',actor (where-is ',object))
        (dprox ',actor ',actor ',object)
        (doit (atrans ',actor ',object ',actor nil))))

(defun dknow (actor info)
  "To find out something: find a friend to tell you"
  (goal-eval actor
             (mloc actor info)
             (gen-plans 'agent
                        (remove actor *personae*)
                        (dknow-plan actor info))))

(defun dknow-plan (actor info)
  `(and (knows-loc ',actor 'agent)
        (or (is-friend-of 'agent ',actor)
            (not (relate ',actor 'agent ',actor 'dominate)))
        (persuade ',actor
                  'agent
                  (mtrans 'agent ',info ',actor 'agent)))) 

(defun dprox (actor object new-object)
  "To move an object (including yourself) to where some other
person or object is: get the first object (if not yourself), then
find out where the second object is and go there with the first
object.  If this doesn't work, try persuading the object to go
there itself."
  (goal-eval actor 
             (is-at object new-object)
             (list (dprox-plan1 actor object new-object)
                   (dprox-plan2 actor object new-object))))

(defun dprox-plan1 (actor object new-object)
  `(and (or (equal ',actor ',object)
            (dprox ',actor ',actor ',object))
        (dknow ',actor (where-is ',new-object))
        (or (equal ',actor ',object)
            (doit (grasp ',actor ',object)))
        (or (is-prox ',actor (loc-name-of ',new-object))
            (and (knows-loc ',actor ',new-object)
                 (doit (ptrans ',actor
                               ',object
                               (knows-loc ',actor ',new-object) 
                               (knows-loc ',actor ',actor)))))
        (or (equal ',actor ',object)
            (doit (un-grasp ',actor ',object)))))

(defun dprox-plan2 (actor object new-object)
  `(and (not (equal ',actor ',object))
        (member ',object *personae*)
        (persuade ',actor
                 ',object
                 (ptrans ',object
                         ',object
                         ',new-object
                         (loc-name-of ',object)))))

;  Subgoals and plans -- persuade, ask, bargain, threaten, and tell:

(defun persuade (actor agent action)
  "You can persuade someone to do something by either asking them,
giving them food or threatening them."
  (goal-eval actor 
             action
             (append (gen-plans 'food 
                                (get-isa 'food agent) 
                                (bargain-plan actor agent action))
                     (list (ask-plan actor agent action))
                     (list (threat-plan actor agent action)))))
 
(defun tell (actor agent info)
  "To tell someone something, go there and say it."
  (goal-eval actor 
             (mloc agent info)
             (list (tell-plan actor agent info))))

(defun tell-plan (actor agent info)
  `(and (dprox ',actor ',actor ',agent)
        (doit (mtrans ',actor ',info ',agent ',actor))))

;  The simulator

(defun doit (cd)
  "doit adds a CD and its consequences to the data base, by calling
assert-fact.  mtranses with '?unspecified have to be filled out, as in
\"Irving told Joe where the honey was\" -- the \"where\" being represented
in the CD with an '?unspecified form."
  (let ((newcd 
         (if (and (equal (header-cd cd) 'mtrans)
                  (knows (cdpath '(actor) cd) 
                         (cdpath '(object) cd)))
           (setrole 'object 
                    (knows (cdpath '(actor) cd) 
                           (cdpath '(object) cd)) 
                    cd)
           cd)))
    (assert-fact newcd)
    newcd))

(defun assert-fact (x)
  "assert-fact is one of the central control functions.  It starts with
one fact, infers the consequences, infers the consequences of the
consequences, etc.  Besides the simple result put in *conseqs*
 (e.g., ptrans changes locs), new states may lead to response actions
 (put in *actions*) or new plans (put in *plans*).  The plans are
done after all the consequences are inferred."
  (setf *actions* nil)
  (setf *plans* nil)
  (forward-chain (list x))
  (mapc #'(lambda (cd) (doit (setrole 'time *default-tense* cd)))
        *actions*)
  (mapc #'eval *plans*))

(defun forward-chain (l)
  (setf *conseqs* nil)
  (mapc #'(lambda (i) 
            (now-knows 'world i nil)
            (conseqs i))
        l)
  (if *conseqs* (forward-chain *conseqs*)))

(defun conseqs (cd)
  "Each act and state is associated with a function for 
calculating the consequences."
  (case (header-cd cd)
    (atrans (atrans-conseqs cd))
    (grasp (grasp-conseqs cd))
    (ingest (ingest-conseqs cd))
    (loc (loc-conseqs cd))
    (mbuild (mbuild-conseqs cd))
    (mloc (mloc-conseqs cd))
    (mtrans (mtrans-conseqs cd))
    (plan (plan-conseqs cd))
    (propel (propel-conseqs cd))
    (ptrans (ptrans-conseqs cd))
    (t nil)))
  
(defun add-conseq (x)
  "add-conseq adds and returns a CD to the list of consequences"
  (push x *conseqs*)
  x)

(defun atrans-conseqs (cd)
  "Consequences of an atrans: everyone in the area notices it and the
resulting change of possesion, the object changes locations, and the
from filler knows he no longer has it."
  (notice (cdpath '(actor) cd) 
          cd)
  (notice (cdpath '(actor) cd) 
          (add-conseq (has (cdpath '(to) cd) 
                           (cdpath '(object) cd))))
  (add-conseq (is-at (cdpath '(object) cd)
                     (cdpath '(to) cd)))
  (if (cdpath '(from) cd)
      (notice (cdpath '(actor) cd)
              (add-conseq (negate (has (cdpath '(from) cd)
                                       (cdpath '(object) cd)))))))

(defun grasp-conseqs (cd)
  "Consequences of a grasp: everyone knows that the actor either has or
(in the case of a tf (transition final or the end of an action) of the
grasp)  doesn't have the object"
  (notice (cdpath '(actor) cd)
          (add-conseq (if (in-mode cd 'tf)
                        (negate (has (cdpath '(actor) cd)
                                     (cdpath '(object) cd)))
                        (has (cdpath '(actor) cd)
                             (cdpath '(object) cd))))))

(defun ingest-conseqs (cd)
  "Consequences of an ingest: everyone knows that the actor 
is no longer hungry or thirsty."
  (notice (cdpath '(actor) cd)
          (add-conseq (state (cdpath '(actor) cd)
                             (if (equal (cdpath '(object) cd) 'water)
                               'thirsty
                               'hungry)
                             'neg))))
 
(defun loc-conseqs (cd)
  "Consequences of a loc change: everyone knows it."
  (notice (cdpath '(actor) cd) cd))

(defun mbuild-conseqs (cd)
  "Consequences of an mbuild: if the object is a causal then a demon
is set up for the actor that will be triggered by the antecedent."
  (if (equal (cdpath '(actor) cd)
             (cdpath '(object conseq actor) cd))
    (put (cdpath '(actor) cd)
         'demons
         (cons (cons (cdpath '(object ante) cd)
                     (cdpath '(object conseq) cd))
               (get (cdpath '(actor) cd) 'demons))))
  nil)

(defun mloc-conseqs (cd)
  "Consequences of an mloc change: check the demons to see if the
learned fact affects the learner.  Also check the reaction list
for general responses to learning such facts."
  (demon-check (cdpath '(val part) cd)
               (cdpath '(con) cd))
  (if (not (member 'neg (cdpath '(con mode) cd)))
    (case (header-cd (cdpath '(con) cd))
      (loc (loc-react cd))
      (mloc (mloc-react cd))
      (hungry (hunger-react cd))
      (thirsty (thirst-react cd))
      (t nil))))

(defun demon-check (who event)
  "Stored under each character is a list of \"demons.\"  A demon is
a CD pattern plus an action.  Whenever the character learns
something this list is checked to see if there is a response to make.
Demons are set up by things like the mbuild in a bargain-plan."
  (put who
       'demons
       (remove-if #'null
                  (mapc #'(lambda (demon)
                            (cond ((unify-cds (car demon) event)
                                   (push (cdr demon) *actions*)
                                   nil)
                                  (t
                                   demon)))
                        (get who 'demons)))))

(defun mtrans-conseqs (cd)
  "Consequences of an mtrans: if there is a ques in the CD mtransed,
and if it is a causal, then it is a bargaining promise; otherwise,
it is a request (assuming the actors in the sub-CD are in the right
places).  If there is no ques in the CD mtransed, then the hearer
knows about the mtrans, and if he believes the speaker, then he
believes what the speaker believes."
  (let ((actor (cdpath '(actor) cd))
        (object (cdpath '(object) cd))
        (hearer (cdpath '(to part) cd)))
    (cond ((member 'ques (cdpath '(object mode) cd))
           (cond ((and (equal (header-cd object) 'cause)
                       (equal actor (cdpath '(object ante actor) cd))
                       (equal hearer (cdpath '(object conseq actor) cd)))
                  (promise-conseqs hearer
                                   (cdpath '(object conseq) cd)
                                   actor
                                   (cdpath '(object ante) cd)))
                 ((equal (cdpath '(object actor) cd) hearer)
                  (request-conseqs actor
                                   hearer
                                   (future (un-question object))))))
          ((not (equal actor hearer))
           (add-conseq (mloc hearer cd))
           (cond ((not (relate hearer actor hearer 'deceive))
                  (add-conseq (mloc hearer (mloc actor object)))))))))

(defun promise-conseqs (x xdo y ydo)
  "Consequences of y asking x to promise to do xdo if y does ydo:
If x deceives y, then after ydo, x will call y stupid, but says
that he will do xdo in return for ydo;
else if x likes y, then x will do xdo after ydo and says so.
Otherwise x says no."
  (let ((a (cause ydo (affirm xdo))))
    (cond ((relate x x y 'deceive)
           (add-conseq (mbuild x
                               (cause ydo
                                      (future (mtrans x
                                                      (state y 'smart 'neg)
                                                      y
                                                      x)))))
           (add-conseq (mtrans x a y x)))
          ((relate x x y 'like)
           (add-conseq (mbuild x a))
           (add-conseq (mtrans x a y x)))
          (t
           (add-conseq (mtrans x (negate a) y x))))))

(defun request-conseqs (x y z)
  "Consequences of x asking y to do z: 
If y doesn't like x or dominates x, then y will say no; otherwise
y will do z."
  (add-conseq (if (or (not (relate y y x 'like))
                      (relate y y x 'dominate))
                (plan y (future (mtrans y (negate z) x y)))
                (plan y z))))

(defun plan-conseqs (cd)
  "Consequences of a plan: If the actor of the plan act is the actor of 
the object of the plan, then add the object to the list of actions."
  (if (equal (cdpath '(actor) cd) (cdpath '(object actor) cd))
    (push (cdpath '(object) cd) *actions*))
  nil)

(defun propel-conseqs (cd)
  "Consequences of a propel: the object struck dies"
  (if (member (cdpath '(to) cd) *personae*)
    (add-conseq (state (cdpath '(to) cd) 'health 'neg))))

(defun ptrans-conseqs (cd)
  "Consequences of a ptrans: location change, for both actor and object."
  (add-conseq (is-at (cdpath '(object) cd) (cdpath '(to) cd)))
  (if (not (equal (cdpath '(actor) cd) (cdpath '(object) cd)))
    (add-conseq (is-at (cdpath '(actor) cd) (cdpath '(to) cd)))))

(defun loc-react (cd)
  "Reactions to learning of a location change: if it's food or water,
check to see if learner is hungry or thirsty."
  (and (or (member (cdpath '(con actor) cd)
                   (get-isa 'food (cdpath '(val part) cd)))
           (equal (cdpath '(con actor) cd) 'water))
       (sgoal-check (cdpath '(val part) cd)
                    (if (equal (cdpath '(con actor) cd) 'water) 
                      'thirsty
                      'hungry))))

(defun sgoal-check (actor scale)
  "If a character is hungry or thirsty, add the appropriate s-goal
to the list of plans."
  (and (in-state actor scale)
       (push (list (if (equal scale 'thirsty)
                     'sthirst
                     'shunger)
                   (list 'quote actor))
             *plans*)))

(defun mloc-react (cd)
  "Reactions to learning that someone has learned something:
if it's someone else, and it's about himself or you believe he
doesn't deceive you, then you believe it too."
  (and (not (equal (cdpath '(val part) cd) (cdpath '(con val part) cd)))
       (or (equal (cdpath '(con con actor) cd) (cdpath '(con val part) cd))
           (not (relate (cdpath '(val part) cd) 
                        (cdpath '(con val part) cd)
                        (cdpath '(val part) cd)
                        'deceive)))
       (add-conseq (mloc (cdpath '(val part) cd)
                         (cdpath '(con con) cd)))))

(defun hunger-react (cd)
  "Reactions to learning that you're hungry: add s-goal to list of plans."
  (push (list 'shunger (list 'quote (cdpath '(con actor) cd))) *plans*))

(defun thirst-react (cd)
  "Reactions to learning you're thirsty: add s-goal to list of plans."
  (push (list 'sthirst (list 'quote (cdpath '(con actor) cd))) *plans*))

(defun notice (who cd)
 "Notice says that everyone in the same location as who knows about CD."
  (let ((where (loc-name-of who)))
    (mapc #'(lambda (persona)
              (if (equal (loc persona) where)
                (add-conseq (mloc persona cd))))
          *personae*)))

(defun addfact (knower cd)
  "Memory functions and pattern matcher
addfact adds a CD to knower's knowledge set.  Also if world
learns a character has died, then the character is removed from the
global list of characters.
The CD is added to the front of the fact list, so that memquery
will get the most recent CD that matches its query pattern.  Older
contradicted facts are still on the list but are not seen."
  (put knower 'facts (cons cd (get knower 'facts)))
  ;;; Now check for deceased people.
  (if (and (equal knower 'world)
           (equal (header-cd cd) 'health)
           (member 'neg (cdpath '(mode) cd)))
    (setf *personae* 
          (remove (cdpath '(actor) cd)
                  *personae*)))
  nil)

(defun is-state (cd)
  "is-state returns non-nil if CD is one of the state forms."
  (member (header-cd cd)
          '(loc 
            mloc 
            cont 
            like 
            deceive 
            dominate 
            hungry 
            thristy 
            health 
            smart)))

(defun now-knows (who what say-flag)
  "now-knows adds what to the data base for who.  It also prints in
English this new fact.  If who = world (a true fact) and what is
an mloc, then save the content of the mloc under the person who
learned it.  If say-flag is t, then mlocs are always generated in
English; otherwise only facts (who = world) are generated.  This
reduces the volume of the output."
  (let ((newwho
         (if (and (equal who 'world) 
                  (equal (header-cd what) 'mloc))
           (cdpath '(val part) what)
           who))
        (newwhat
         (if (and (equal who 'world) 
                  (equal (header-cd what) 'mloc))
           (cdpath '(con) what)
           what)))
    (if (or say-flag 
            (equal newwho 'world))
      (say (mloc newwho newwhat)))
    (addfact newwho newwhat)))

(defun knows (knower fact)
  "knows(knower,fact) returns fact if fact is in data base for knower:
-- if fact = knows(knower,subfact), assume everyone knows what they
   know and look up subfact,
-- if fact has a ?unspec, then return the filler that replaces
   the ?unspec in the data base."
  (let ((newfact
         (if (and (equal (header-cd fact) 'mloc)
                  (equal (cdpath '(val part) fact) knower))
           (cdpath '(con) fact)
           fact)))
  (memquery knower newfact)))

(defun knows-loc (knower object)
  (cdpath '(val) (knows knower (where-is object))))

(defun knows-owner (knower object)
  (cdpath '(val) (knows knower (who-has object))))

(defun knows-if (knower cd)
  (cdpath '(mode) (knows knower (setrole 'mode '?unspecified cd))))

(defun memquery (knower pat)
  "memquery find the first item in knower's data base that matches fact."
  (car (pat-member pat (get knower 'facts))))

(defun pat-member (pat cd-list)
  "pat-member finds the first item in cd-list that matches
pat and returns cd-list from that item on."
  (if cd-list
    (let ((cd (car cd-list)))
      (if (unify-cds pat cd)
        cd-list
        (pat-member pat (cdr cd-list))))))

(defun has-goal-of (actor pat)
  "Returns non-nil if actor has goal."
  (car (pat-member pat (get actor 'goals))))

(defun gets-new-goal-of (actor goal)
  "Adds goal to data base."
  (put actor 'goals (cons goal (get actor 'goals)))
  (say (wants actor goal)))

(defun forgets-goal-of (actor goal)
  "Removes goal from data base"
  (let ((goal-to-be-forgotten (has-goal-of actor goal)))
    (put actor
         'goals
         (remove-if #'(lambda (g)
                        (equal g goal-to-be-forgotten))
                   (get actor 'goals)))))

(defun in-state (x st)
  "Returns non-nil if x is in a state, e.g., hungry."
  (find-out 'world (state x st 'pos)))

(defun relate (x y z rel)
  "Returns non-nil if X believes that y relates to z in a certain way.
Usually either y or z is x."
  (find-out x (relation y z rel 'pos)))

(defun find-out (who cd)
  "Looks up CD in the data base for who.  If there, return non-nil if
the CD is not a negative fact.  If not there, ask the user at the
terminal and save the result.  Note that the generator is used to
ask questions.

find-out is used to determine if a given character is in a
given state (e.g., is the character hungry or thirsty) and is
also used to determine how two characters relate to on another
(e.g., do they like one another?, does one have a tendency to
deceive the other, etc.)."
  (let ((mode (knows-if who cd)))
    (cond (mode 
           (member 'pos mode))
          (t
           (say (mloc who cd))
           (format t "~% [Y/N]? ~%>")
           (let ((answer (equal (read) 'y)))
             (addfact who
                      (setrole 'mode
                               (list (if answer 'pos 'neg))
                               cd))
             answer)))))

(defun is-friend-of (x y)
  "True if y thinks x is a friend of his."
  (and (not (equal x y))
       (relate y x y 'like)))

(defun loc (x)
  "Returns location of x."
  (knows-loc 'world x))

(defun is-prox (x y)
  "True if x and y are in the same place."
  (equal (loc-name-of x)
         (loc-name-of y)))

(defun is-true (cd)
  "A CD is true if it's an mloc and the content is in the person's
data base, or it's in the data base for world."
  (if (equal (header-cd cd) 'mloc)
    (knows (cdpath '(val part) cd) (cdpath '(con) cd))
    (knows 'world cd)))

(defun loc-name-of (x)
  "loc-name-of returns the real location of x.  This may involve going
up several levels -- e.g., when Joe takes a worm, its location is
stored as joe, but its real location is the location Joe is at."
  (let ((loc-of-x (loc x)))
    (cond ((member x *all-locations*)
           x)
          ((member loc-of-x *all-locations*)
           loc-of-x)
          ;;; If something isn't anywhere in particular, 
          ;;; then it on the ground.
          ((null loc-of-x)
           'ground)
          (t
           (loc-name-of loc-of-x)))))

(defun get-isa (x y)
  "get-isa is like get but checks is-a node for x if x has no y property."
  (or (get y x)
      (get (get y 'is-a) x)))

;  Functions to build CD forms

;  Acts

(defun atrans (actor object to from)
  (list 'atrans 
        (list 'actor actor)
        (list 'object object)
        (list 'to to)
        (list 'from from)))

(defun cause (x y)
  (list 'cause
        (list 'ante x)
        (list 'conseq y)))

(defun grasp (actor object)
  (list 'grasp
        (list 'actor actor)
        (list 'object object)))

(defun un-grasp (actor object)
  (tf (grasp actor object)))

(defun ingest (actor object)
  (list 'ingest
        (list 'actor actor)
        (list 'object object)))

(defun mbuild (actor object)
  (list 'mbuild
        (list 'actor actor)
        (list 'object object)))

(defun mtrans (actor object to from)
  (list 'mtrans 
        (list 'actor actor)
        (list 'object object)
        (list 'to (list 'cp (list 'part to)))
        (list 'from from)))

(defun plan (actor object)
  (list 'plan
        (list 'actor actor)
        (list 'object object)))

(defun propel (actor object to)
  (list 'propel 
        (list 'actor actor)
        (list 'object object)
        (list 'to to)))

(defun ptrans (actor object to from)
  (if to
    (list 'ptrans 
          (list 'actor actor)
          (list 'object object)
          (list 'to to)
          (list 'from from))))

(defun wants (actor goal)
  (list 'want
        (list 'actor actor)
        (list 'object goal)))

;  States

(defun has (actor object)
  (list 'cont
        (list 'actor object)
        (list 'val actor)))

(defun is-at (actor loc)
  (list 'loc
        (list 'actor actor)
        (list 'val loc)))

(defun mloc (actor con)
  (list 'mloc
        (list 'con con)
        (list 'val (list 'cp (list 'part actor)))))

(defun state (actor st mode)
  (list st
        (list 'actor actor)
        (list 'mode (list mode))))

(defun relation (actor object rel mode)
  (list rel
        (list 'actor actor)
        (list 'to object)
        (list 'mode (list mode))))

(defun where-is (x)
  (list 'loc
        (list 'actor x)
        (list 'val '?unspecified)))

(defun who-has (x)
  (list 'cont
        (list 'actor x)
        (list 'val '?unspecified)))

;  Mode functions

(defun mode (cd)
  (cdpath '(mode) cd))

;  Affirm/Negate set the mode of a CD to true/false.

(defun affirm (cd)
  (if (member 'pos (mode cd))
    cd
    (setrole 'mode (cons 'pos (remove 'neg (mode cd))) cd)))

(defun negate (cd)
  (if (member 'neg (mode cd))
    (affirm cd)
    (setrole 'mode (cons 'neg (remove 'pos (mode cd))) cd)))
 
(defun maybe (cd)
  "maybe makes a CD hypothetical -- doesn't matter if it's true or false."
  (if (member 'maybe (mode cd))
    cd
    (setrole 'mode (cons 'maybe (mode cd)) cd)))

(defun question (cd)
  "question/un-question make a CD a question/non-question -- doesn't
matter if it's true or false."
  (if (member 'ques (mode cd))
    cd
    (setrole 'mode (cons 'ques (mode cd)) cd)))

(defun un-question (cd)
  (setrole 'mode (remove 'ques (mode cd)) cd))

(defun tf (cd)
  "tf adds \"transition final\" to a CD -- doesn't matter if it's true or false."
  (if (member 'tf (mode cd))
    cd
    (setrole 'mode (cons 'tf (mode cd)) cd)))

(defun future (cd)
  "future sets a CD to a future time."
  (setrole 'time 'future cd))

;  Path
;
;  cdpath finds the filler at the end of the role list in a CD.
;
;  For example, if
;  CD = (mtrans (actor joe)
;               (object (ptrans (actor joe) 
;                               (object worm)
;                               (from joe)
;                               (to irving))))
;  then
;  (cdpath '(actor) cd) returns joe;
;  (cdpath '(object) cd) returns (ptrans (actor joe) 
;                                        (object worm)
;                                        (from joe)
;                                        (to irving));
;  (cdpath '(object object) cd) returns worm.
;
;  If a role doesn't exist in a CD form, then cdpath returns nil.

(defun cdpath (rolelist cd)
  "cdpath finds the filler at the end of the role list in a CD.
If a role doesn't exist in a CD form, then cdpath returns nil."
  (if (null rolelist)
    cd
    (cdpath (cdr rolelist) (filler-role (car rolelist) cd))))


;  micro-mumble: micro English generator

(defun say (cd)
  "say prints a CD as an English sentence.  If CD is an mloc of the
world, then only the fact itself is said, otherwise the whole mloc
is used.  The original CD is returned.  say1 is called with the 
infinitive flag off and the say-subject flag on."
  (let ((cd-to-be-said (if (unify-cds '(mloc (val (cp (part world)))) cd)
                         (cdpath '(con) cd)
                         cd)))
    (flet ((say-it ()
             (format t "~%")
;             (concatenatef *outputstring* "~%")
             (say1 cd-to-be-said 
                   (or (cdpath '(time) cd-to-be-said)
                       *default-tense*)
                   nil
                   t)
             (format t ".")
             (concatenatef *outputstring* ".~%")))
      (say-it))
    cd))

(defun say1 (cd tense inf subj)
  "say1 prints cd according to the program under the head predicate.
If no program is there, the CD is printed with <>s around it.

These generation programs are lists of expressions to be evaluated.
Attached to primative acts, they are normally concerned with
generating subject-verb-object clauses.  Since some of the acts,
such as mtrans, want and plan, take subclauses, the generator has to
be recursive, so that the atrans program that generates the clause
\"Joe gave Irving the worm\" can also generate the subclause in
\"Joe planned to give Irving the worm.\" This means that the programs have
to know when to say or not say the subject, when to use the 
infinitive form, and what tense to use.
  subj = true means print the subject,
  inf = true means use the infinitive form,
  tense is set to either past, present, or future, or cond (for
          conditional, i.e., hypothetical)"
  (let ((say-fun (get (header-cd cd) 'say-fun)))
    (if say-fun 
      (apply say-fun (list cd tense inf subj))
      (progn
        ; (break)
        (format t "~% < ~s > " cd)
        (concatenatef *outputstring* (format nil " < ~s > ~%" cd)))))) ; RJ

(defun subclause (cd word rolelist tense)
  "subclause recursively calls say1 with the subconcept at the 
endpoint of rolelist.  word, if non-nil, starts the subclause,
unless relative-pronoun has a better idea.  Tense is calculated 
by sub-tense."
  (when word
    (concatenatef *outputstring* (format nil "~s " (or (relative-pronoun rolelist cd)
    										word)))
    (format t "~s " (or (relative-pronoun rolelist cd)
                          word)))
  (let ((subcd (cdpath rolelist cd)))
    (say1 subcd (sub-tense tense subcd) nil t)))

(defun sub-tense (tense subcd)
  "sub-tense is given a tense and a CD and picks the tense to use.
The given tense is used, except with states (i.e., don't
say \"he told him where the honey would be\" even though conceptually
that's right), and with past statements about the future (i.e., say
\"he said he would\" rather than \"he said he will\")."
  (cond ((is-state subcd)
         *default-tense*)
        ((and (equal tense 'past)
              (equal (cdpath '(time) subcd) 'future))
         'cond)
        (t
         tense)))

(defun relative-pronoun (rolelist cd)
  "relative-pronoun returns the word to start the subclause
for the CD at the end of the CD role path."
  (let ((subcd (cdpath rolelist cd)))
    (cond ((and (equal (header-cd subcd) 'loc)
                (pcvar-p (cdpath '(val) subcd)))
           'where)
          ((pcvar-p (next-subject cd)) 
           'who)
          (t
           nil))))

(defun next-subject (cd)
  "next-subject returns the subject of a subconcept, which is normally
the actor slot, except for cont (where it's in the val slot) and
mloc (where it's in the part slot of the val slot)."
  (let ((subcd (cdpath '(object) cd)))
    (cdpath (case (header-cd subcd)
              (cont '(val))
              (mloc '(val part))
              (t '(actor)))
            subcd)))

(defun infclause (cd rolelist subj-flag tense)
  "infclause calls recursively say1 with the subconcept at the
endpoint of rolelist.  An infinitive is printed, and the subject
is suppressed."
  (say1 (cdpath rolelist cd) tense t subj-flag))

;  Store say-funs for each of the CD forms

(defun say-atrans (cd tense inf subj)
  "atrans may go to either \"take\" (if actor = to) or \"give.\""
  (cond ((equal (cdpath '(actor) cd) (cdpath '(to) cd))
         (say-subj-verb cd tense inf subj '(actor) 'take)
         (format t " ")
         (concatenatef *outputstring* " ")
         (say-filler cd '(object))
         (say-prep cd 'from '(from) t))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'give)
         (format t " ")
         (concatenatef *outputstring* " ")
         (say-filler cd '(to))
         (format t " ")
         (concatenatef *outputstring* " ")
         (say-filler cd '(object)))))

(put 'atrans 'say-fun 'say-atrans)

(defun say-mtrans (cd tense inf subj)
  "mtrans may go to either \"ask whether\" or \"tell that\""
  (cond ((member 'ques (cdpath '(object mode) cd))
         (say-subj-verb cd tense inf subj '(actor) 'ask)
         (format t " ")
         (concatenatef *outputstring* " ")
         (say-filler cd '(to part))
         (format t " ")
         (concatenatef *outputstring* " ")
         (subclause cd 'whether '(object) 'cond))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'tell)
         (format t " ")
         (concatenatef *outputstring* " ")
         (say-filler cd '(to part))
         (format t " ")
         (concatenatef *outputstring* " ")
         (subclause cd 'that '(object) (cdpath '(time) cd)))))

(put 'mtrans 'say-fun 'say-mtrans)

(defun say-ptrans (cd tense inf subj)
  "ptrans may go to either \"go\" or \"move.\""
  (cond ((equal (cdpath '(actor) cd)
                (cdpath '(object) cd))
         (say-subj-verb cd tense inf subj '(actor) 'go))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'move)
         (format t " ")
         (concatenatef *outputstring* " ")
         (say-filler cd '(object))))
  (say-prep cd 'to '(to) t))

(put 'ptrans 'say-fun 'say-ptrans)

(defun say-mbuild (cd tense inf subj)
  "mbuild may go to either \"decide to\" or \"decide that.\""
  (say-subj-verb cd tense inf subj '(actor) 'decide)
  (format t " ")
  (concatenatef *outputstring* " ")
  (cond ((equal (cdpath '(actor) cd)
                (cdpath '(object actor) cd))
         (infclause cd '(object) nil 'future))
        (t
         (subclause cd 'that '(object) 'future))))

(put 'mbuild 'say-fun 'say-mbuild)

(defun say-propel (cd tense inf subj)
  "propel goes to strike"
  (say-subj-verb cd tense inf subj '(actor) 'strike)
  (format t " ")
  (concatenatef *outputstring* " ")
  (say-filler cd '(to)))

(put 'propel 'say-fun 'say-propel)

(defun say-grasp (cd tense inf subj)
  "grasp may go to either \"let go of\" or \"grab.\""
  (cond ((in-mode cd 'tf)
         (say-subj-verb cd tense inf subj '(actor) 'let)
         (format t " GO OF")
         (concatenatef *outputstring* " GO OF"))
        (t
         (say-subj-verb cd tense inf subj '(actor) 'grab)))
  (say-filler cd '(object)))

(put 'grasp 'say-fun 'say-grasp)

(defun say-ingest (cd tense inf subj)
  "ingest may go to either \"eat\" or \"drink.\""
  (say-subj-verb cd tense inf subj '(actor)
                 (if (equal (cdpath '(object) cd) 'water)
                   'drink
                   'eat))
  (format t " ")
  (concatenatef *outputstring* " ")
  (say-filler cd '(object)))

(put 'ingest 'say-fun 'say-ingest)

(defun say-plan (cd tense inf subj)
  "plan goes to \"plan.\""
  (say-subj-verb cd tense inf subj '(actor) 'plan)
  (format t " ")
  (concatenatef *outputstring* " ")
  (infclause cd '(object) nil 'future))

(put 'plan 'say-fun 'say-plan)

(defun say-want (cd tense inf subj)
  "want goes to \"want to\" -- the third argument of infclause is set to 
true if the subject of the subclause is different that the subject
of the main clause."
  (say-subj-verb cd tense inf subj '(actor) 'want)
  (format t " ")
  (concatenatef *outputstring* " ")
  (infclause cd 
             '(object)
             (not (equal (cdpath '(actor) cd)
                         (next-subject cd)))
             'future))

(put 'want 'say-fun 'say-want)

(defun say-loc (cd tense inf subj)
  "loc goes to \"be near.\""
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (or (pcvar-p (cdpath '(val) cd))
      (say-prep cd 'near '(val) t)))

(put 'loc 'say-fun 'say-loc)

(defun say-cont (cd tense inf subj)
  "cont goes to \"have.\""
  (say-subj-verb cd tense inf subj '(val) 'have)
  (format t " ")
  (concatenatef *outputstring* " ")
  (say-filler cd '(actor)))

(put 'cont 'say-fun 'say-cont)

(defun say-mloc (cd tense inf subj)
  "mloc may go to either \"know that\", \"know whether\", or \"think that.\""
  (say-subj-verb cd 
                 tense 
                 inf 
                 subj 
                 '(val part)
                 (if (or (relative-pronoun '(con) cd)
                         (is-true (cdpath '(con) cd)))
                   'know
                   'think))
  (format t " ")
  (concatenatef *outputstring* " ")
  (subclause cd 'that '(con) *default-tense*))

(put 'mloc 'say-fun 'say-mloc)

(defun say-health (cd tense inf subj)
  "health goes to \"be alive\""
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (concatenatef *outputstring* " ")
  (format t "ALIVE")
  (concatenatef *outputstring* "ALIVE"))

(put 'health 'say-fun 'say-health)

(defun say-smart (cd tense inf subj)
  "smart goes to \"be bright\""
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (concatenatef *outputstring* " ")
  (format t  "BRIGHT")
  (concatenatef *outputstring* "BRIGHT"))

(put 'smart 'say-fun 'say-smart)

(defun say-hungry (cd tense inf subj)
  "hungry goes to \"be hungry\""
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (concatenatef *outputstring* " ")
  (format t  "HUNGRY")
  (concatenatef *outputstring* "HUNGRY"))

(put 'hungry 'say-fun 'say-hungry)

(defun say-thirsty (cd tense inf subj)
  "thirsty goes to \"be thirsty\""
  (say-subj-verb cd tense inf subj '(actor) 'be)
  (format t " ")
  (concatenatef *outputstring* " ")
  (format t "THIRSTY")
  (concatenatef *outputstring* "THIRSTY"))

(put 'thirsty 'say-fun 'say-thirsty)

(defun say-cause (cd tense inf subj)
  "cause may go to either \"x if y\" or \"if x then y\""
  (declare (ignore inf))
  (declare (ignore subj))
  (cond ((in-mode cd 'ques)
         (subclause cd nil '(conseq) 'future)
         (format t " IF ")
         (concatenatef *outputstring* " IF ")
         (subclause cd nil '(ante) (case tense
                                     (figure 'present)
                                     (cond *default-tense*)
                                     (t tense))))
        (t
         (format t "IF ")
         (concatenatef *outputstring* "IF ")
         (subclause cd nil '(ante) 'future)
         (format t " THEN ")
         (concatenatef *outputstring* " THEN ")
         (subclause cd nil '(conseq) 'cond))))

(put 'cause 'say-fun 'say-cause)

(defun say-like (cd tense inf subj)
  "like goes to \"like\""
  (say-subj-verb cd tense inf subj '(actor) 'like)
  (format t " ")
  (concatenatef *outputstring* " ")
  (say-filler cd '(to)))

(put 'like 'say-fun 'say-like)

(defun say-dominate (cd tense inf subj)
  "dominate goes to \"dominate\""
  (say-subj-verb cd tense inf subj '(actor) 'dominate)
  (format t " ")
  (concatenatef *outputstring* " ")
  (say-filler cd '(to)))

(put 'dominate 'say-fun 'say-dominate)

(defun say-deceive (cd tense inf subj)
  "deceive goes to \"deceive\""
  (say-subj-verb cd tense inf subj '(actor) 'deceive)
  (format t " ")
  (concatenatef *outputstring* " ")
  (say-filler cd '(to)))

(put 'deceive 'say-fun 'say-deceive)

(defun say-filler (cd rolelist)
  "say-filler prints the CD at the end of a CD role path"
  (say-pp (cdpath rolelist cd)))

(defun say-pp (cd)
  "say-pp prints a CD (adds \"the\" to object)."
  (if (member cd *all-objects*)
;    (format t "THE ")
    (concatenatef *outputstring* "THE "))
  (format t "~s" cd)
  (concatenatef *outputstring* (format nil "~s" cd)))

(defun say-prep (cd prep rolelist &optional space)
  "say-prep prints a preposition plus a CD at the end of a role path,
if any exists."
  (let ((subcd (cdpath rolelist cd)))
    (cond (subcd
           (when space
             (format t " ")
             (concatenatef *outputstring* " "))
           (format t "~s " prep)
           (concatenatef *outputstring* (format nil "~s " prep))
           (say-pp subcd)))))

(defun in-mode (cd x)
  "in-mode tests whether x is in CD's mode."
  (member x (cdpath '(mode) cd)))

(defun say-neg (cd &optional space0 space1)
  "say-neg prints \"not\" if CD is negative."
  (when (in-mode cd 'neg)
    (when space0
      (format t " ")
      (concatenatef *outputstring* " "))
    (format t "NOT")
    (concatenatef *outputstring* "NOT")
    (when space1
      (format t " ")
      (concatenatef *outputstring* " "))))

(defun say-subj-verb (cd tense inf subj rolelist infinitive)
  "say-subj-verb prints the subject (unless suppressed by
subj = nil, infinitives, or an ?unspec as the subject) and verb, 
with auxillary and tensing, if any.  Note that future tense is 
treated as an auxillary."
  (let ((subject (cdpath rolelist cd)))
    (cond (inf
           (when subj (say-pp subject) (format t " "))
           (when subj (say-pp subject) (concatenatef *outputstring* " "))
           (say-neg cd nil t)
           (format t "TO ~s" infinitive)
           (concatenatef *outputstring* (format nil "TO ~s" infinitive)))
          (t
           (when (not (pcvar-p subject)) 
             (say-pp subject)
             (format t " ")
             (concatenatef *outputstring* " "))
           (let ((plural 
                  (get subject 'plural))
                 (auxilary
                  (cond ((in-mode cd 'maybe)
                         'might)
                        ((equal tense 'future)
                         (if (equal *default-tense* 'past)
                             'would
                           'will))
                        ((equal tense 'cond)
                         'would)
                        ((and (in-mode cd 'neg)
                              (not (equal infinitive 'be)))
                         'do))))
             (cond (auxilary
                    (say-tense cd tense inf subj auxilary plural)
                    (say-neg cd t nil)
                    (format t " ~s" infinitive)
                    (concatenatef *outputstring* (format nil " ~s" infinitive)))
                   (t
                    (say-tense cd tense inf subj infinitive plural)
                    (when (equal infinitive 'be)
                      (say-neg cd t nil)))))))))

(defun say-tense (cd tense inf subj infinitive plural)
  "say-tense prints a verb, with tense and number inflection.
Conjugations of irregular verbs are stored under the past and present
properties of the verb, in the format (singular plural) for each.
For regular verbs, say-tense adds \"d\", \"ed\", or \"s\" as appropriate."
  (declare (ignore cd))
  (declare (ignore inf))
  (declare (ignore subj))
  (let ((tense-forms (get infinitive tense)))
    (cond (tense-forms
    	   (concatenatef *outputstring* (format nil "~s" (if plural
    	   										(cadr tense-forms)
    	   										(car tense-forms))))
           (format t "~s" (if plural
                            (cadr tense-forms)
                            (car tense-forms))))
          (t
           (format t "~s" infinitive)
           (concatenatef *outputstring* (format nil "~s" infinitive))
           (case tense
             (past
              (if (not (or (equal (lastchar infinitive) #\E)
                           (equal (lastchar infinitive) #\e)))
;                (format t "E")
                (concatenatef *outputstring* "E"))
;              (format t "D")
              (concatenatef *outputstring* "D"))
             (present
              (if (not plural)
                (format t "S")
                (concatenatef *outputstring* "S"))))))))

(defun lastchar (x)
  "lastchar returns that last character in x"
  (car (last (explode x))))

(defun explode (x)
  (coerce (princ-to-string x) 'list))

;  Generator Dictionary
;
;  Set the past and/or present tenses for irregular verbs.
;  Each tense is of the form (singular plural).

(put 'be 'past '(was were))
(put 'be 'present '(is are))
(put 'do 'past '(did did))
(put 'do 'present '(does do))
(put 'drink 'past '(drank drank))
(put 'eat 'past '(ate ate))
(put 'give 'past '(gave gave))
(put 'go 'past '(went went))
(put 'go 'present '(goes go))
(put 'grab 'past '(grabbed grabbed))
(put 'have 'past '(had had))
(put 'have 'present '(has have))
(put 'know 'past '(knew knew))
(put 'let 'past '(let let))
(put 'might 'past '(might might))
(put 'might 'present '(might might))
(put 'plan 'past '(planned planned))
(put 'strike 'past '(struck struck))
(put 'take 'past '(took took))
(put 'tell 'past '(told told))
(put 'think 'past '(thought thought))

;  Berries is the only plural in the current set-up.
(put 'berries 'plural t)

;  CD Functions

(defun is-cd-p (x)
  "is-cd-p determines whether a given sexpr is a CD."
  (and (listp x)
       (atom (header-cd x))
       (list-of-role-filler-pairs-p (roles-cd x))))

(defun list-of-role-filler-pairs-p (x)
  (or (null x)
      (and (listp x)
           (listp (car x))
           (atom (role-pair (car x)))
           (list-of-role-filler-pairs-p (cdr x)))))

(defun header-cd (x)
  "header-cd gets the head act of a CD form."
  (car x))

(defun roles-cd (x)
  "roles-cd gets the list of role-pairs of a CD form."
  (cdr x))

(defun role-pair (x)
  "Role-pairs have the form (role filler).
role-pair returns the role."
  (car x))

(defun filler-pair (x)
  "filler-pair returns the filler."
  (cadr x))

(defun filler-role (role cd)
  "A filler for a role is found by looking for the role name in the CD,
and returning the filler if a pair is found."
  (if (listp cd)
    (let ((pair (assoc role (roles-cd cd))))
      (if pair (filler-pair pair)))))

(defun setrole (role filler cd)
  "setrole makes a new CD form with (role filler) added
or replacing the old (role ...) pair."
  (cons (header-cd cd)
        (cons (list role filler)
              (delete-if #'(lambda (pair)
                             (eq (car pair) role))
                         (roles-cd cd)))))

;  Pattern Unifier
;  This unifier is an adapted version of the unify function which appears
;  in the book _Artificial_Intelligence_Programming_ (2nd ed.)
;  Eugene Chaniak, Drew McDermott, and James Meehan.

(defun unify (Pat1 Pat2)
  (unify-1 Pat1 Pat2 NIL))

(defun unify-1 (Pat1 Pat2 Sub)
  (cond ((pcvar-p Pat1)
         (var-unify Pat1 Pat2 Sub))
        ((pcvar-p Pat2)
         (var-unify Pat2 Pat1 Sub))
        ((atom Pat1)
         (cond ((eql Pat1 Pat2) (list Sub))
               (T NIL)))
        ((atom Pat2) 
         NIL)
        (T
         (mapcan #'(lambda (Sub)
                     (unify-1 (cdr Pat1) (cdr Pat2) Sub))
                 (unify-1 (car Pat1) (car Pat2) Sub)))))

(defvar *OccursCheck-P* T)

(defun var-unify (PCVar Pat Sub)
  (cond ((eql PCVar Pat)
         (list Sub))
        (T
         (let ((Binding (pcvar-binding PCVar Sub)))
           (cond (Binding
                  (unify-1 (binding-value Binding) Pat Sub))
                 ((and *OccursCheck-P*
                       (occurs-in-p PCVar Pat Sub))
                  NIL)
                 (T
                  (list (extend-binding PCVar Pat Sub))))))))

(defun occurs-in-p (PCVar Pat Sub)
  (cond ((pcvar-p Pat)
         (or (eq PCVar Pat)
             (let ((Binding (pcvar-binding Pat Sub)))
               (and Binding
                    (occurs-in-p PCVar (binding-value Binding) Sub)))))
        ((atom Pat)
         NIL)
        (T
         (or (occurs-in-p PCVar (car Pat) Sub)
             (occurs-in-p PCVar (cdr Pat) Sub)))))

(defun pcvar-binding (PCVar AList)
  (assoc PCVar AList))

(defun extend-binding (PCVar Pat AList)
  (cons (list PCVar Pat)
        AList))

(defun binding-value (Binding) (cadr Binding))

(defun pcvar-value (Pat Sub)
  (let ((Binding (pcvar-binding Pat Sub)))
    (cond ((null Binding) 
           Pat)
          (T
           (let ((Value (binding-value Binding)))
             (cond ((eql Value Pat)
                    Pat)
                   (T
                    (replace-variables Value Sub))))))))

(defun replace-variables (Pat Sub)
  (cond ((pcvar-p Pat)
         (pcvar-value Pat Sub))
        ((atom Pat)
         Pat)
        (T
         (cons (replace-variables (car Pat) Sub)
               (replace-variables (cdr Pat) Sub)))))

(defun instantiate (Pat Subs)
  (cond ((pcvar-p Pat)
         (let ((Entry (assoc Pat Subs)))
           (if Entry 
             (instantiate (cadr Entry) Subs)
             Pat)))
        ((atom Pat)
         Pat)
        (T
         (cons (instantiate (car Pat) Subs)
               (instantiate (cdr Pat) Subs)))))

;  CD Unifier
;  This replaces the less-general CD pattern matcher that was
;  used in the original Micro-Talespin program.  This unifier
;  allows pattern variables to appear on both of the
;  expressions to be compared while a pattern matcher
;  only allows variables to appear in one of the expressions.

(defun unify-cds (cd1 cd2)
  (unify-cds-1 cd1 cd2 nil))

(defun unify-cds-1 (cd1 cd2 sub)
  (and (eq (header-cd cd1) (header-cd cd2))
       (unify-pairs (roles-cd cd1) (roles-cd cd2) sub)))

(defun unify-pairs (pairs1 pairs2 sub)
  "unify-pairs sees if the roles and fillers of a CD can
be matched together.  It is more complicated than the
function unify-1 given above because (1) the role-filler pairs
do not need to be in the same order in the two CDs being
compared; (2) a missing pair in one CD means that that CD
is more general than the other CD and can, thus, be matched
against it; and, finally, (3) the filler of a pair can be a CD,
and most fillers which are lists are CDs, however, fillers which
are \"modes\" are the exception; they are fillers which are lists,
but are not CDs, so a special exception has to be made for them
in the unification procedure below."
  (if (or (null pairs1) (null pairs2))
    (list sub)
    (let* ((role 
            (role-pair (car pairs1)))
           (pair-from-pairs2
            (assoc role pairs2))
           (rest-of-pairs-from-pairs2
            (remove-if #'(lambda (pair)
                           (equal (role-pair pair) role))
                       pairs2))
           (newsubs
            (cond ((eq role 'mode)
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                   ((and pair-from-pairs2
                        (or (pcvar-p (cadr pair-from-pairs2))
                            (atom (cadr pair-from-pairs2))))
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                  ((and pair-from-pairs2
                        (or (pcvar-p (cadr (car pairs1)))
                            (atom (cadr (car pairs1)))))
                   (unify-1 (car pairs1) pair-from-pairs2 sub))
                  (pair-from-pairs2
                   (unify-cds-1 (car pairs1) pair-from-pairs2 sub))
                  (t
                   (list sub)))))
      (mapcan #'(lambda (newsub)
                  (unify-pairs (cdr pairs1)
                               rest-of-pairs-from-pairs2
                               newsub))
              newsubs))))

;  Done loading
(format t "~%Done loading Micro-Talespin")


