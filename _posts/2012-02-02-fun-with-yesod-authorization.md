---
layout: post
title: More Fun with Yesod Authorization
---

I was inspired by Felipe Lessa's post about [abstracting Yesod
permissions](http://blog.felipe.lessa.nom.br/?p=7). Thanks for writing it,
Felipe! I had only just discovered `isAuthorized`, and that post made me all
the more excited to go about refactoring my authorization code.

Unfortunately I had a little trouble working Felipe's method into my own
project. This was probably due in no small part to my too-recent discovery of
`isAuthorized` and the `AuthResult` type. After much tinkering, I narrowed the
difficulties down to just one (and a half) points of contention:

1. Permission values are taken to mean actions: "Permission to &lt;do
action&gt;". But RESTful routes are *also* taken to mean actions. The
arguments `BlogR True` clearly mean "Write to BlogR." Thus,
`permissionsRequiredFor` provides superfluous information. `Post` == `BlogR
True` == "Post a Blog".
2. (Actually 1.5) To paraphrase hlint, "Why not foldM?" :)

To resolve the first point, I propose this:

{% template haskell %}
-- Permission replacement
data Credential = LoggedIn | IsAdmin

-- permissionsRequiredFor replacement
--
requiredCredentials :: Route Blog -> Bool -> [Credential]
requiredCredentials BlogR      True = [IsAdmin]
requiredCredentials (EntryR _) True = [LoggedIn]
requiredCredentials _          _    = []
{% endtemplate %}

Note that this new function does the same thing as the one it replaces, but
reading it gives us more information: we know the action from the route, and
we also know what credentials a user needs to perform that action.

If that doesn't immediately seem crucial, consider that many actions may be satisfied by the same credential. If I have a box with a key, it is assumed that I may do anything with the _contents_ of that box provided I have the key. In other words,

{% template haskell %}
data Credentials = HasKey

requiredCredentials PutInBoxR ... = [HasKey BoxID]
requiredCredentials TakeFromBoxR  = [HasKey BoxID]
{% endtemplate %}

With Permissions, either you write separate permissions that do the same thing (GetBox, PutBox), or you break your semantic model and use a single Permission for both ("permission to Box?")

For the second point, note that `isAuthorizedTo` can be written like so (with a
couple more name changes to sound sensible with `Credential`):

{% template haskell %}
isAuthorizedTo :: Maybe (Entity User)
               -> [Credential]
               -> YesodDB sub Blog AuthResult
Nothing `isAuthorizedTo` _  = return AuthenticationRequired
Just u  `isAuthorizedTo` ps = foldM hasCred Authorized ps
  where
    hasCred Authorized p = u `hasCredential` p
    hasCred badAuth    _ = return badAuth
{% endtemplate %}
