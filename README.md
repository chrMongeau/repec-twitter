# RePEc economists on Twitter

This repository has two (main) R scripts:

* list.R
* network.R

All is [CC-BY-NC-SA](http://creativecommons.org/licenses/by-nc-sa/4.0/).

In order to use the Twitter API, you need to create an app. Follow the
instructions at <http://rtweet.info/articles/auth.html>.

## list.R

This R script is used to update the list at
<https://twitter.com/chrMongeau/lists/repec-twitter>.
It downloads the list members and the list of all the economists listed at
<https://ideas.repec.org/i/etwitter.html>. New members are added to the
Twitter list (when the list is updated, a reply is sent to the original
tweet: <https://twitter.com/chrMongeau/status/706873909484851200>).

## network.R

This R script creates an interactive network of RePEc economists on Twitter.
It download the members listed at
<https://twitter.com/chrMongeau/lists/repec-twitter> and retrieve some
information about them from their pages at <https://ideas.repec.org/>.
Specifically, it extracts the following information:

* name;
* NEP (New Economic Papers) fields (see below);
* main NEP field group (see below);
* affiliation (see below);
* number of articles;
* number of papers;
* number of software components.

Moreover, the following information from Twitter is retrieved:

* date the user joined Twitter;
* location;
* whether the account is protected;
* numer of followers;
* number of "friends";
* number of favourites;
* the number of statuses;
* the number of statuses per day;
* language;
* profile pic.

The network is built by querying the list of users' "friends", which in
Twitter parlance means "people followed". If person A follows person B, a
(directed) link is created. Edges are weigthed and their value is given by
nodes similarity calculated from NEP fields (see below).

### NEP fields

These fields (see: <https://ideas.repec.org/n/>) are extracted in order to
compute the (cosine) similarity among economists. This measure -- which goes
to zero (no fields in common) to 1 (all fields in common) -- is then used to
give a weight to the network. This is useful for grouping economists by
research fields, as the layout algorithm (see beolow) accounts for weights.

### Main NEP field

The field with the highest frequency is assigned as "field". Economists with
more than one main field (i.e., cases with ties) are given a random field
chosen between the main fields (this is probably not optimal, but the option
was to assign a NA to the field, which doesn't seems a wise thing to do).

### Affiliation

Affiliation is the one with the highest percentage. If no percentage is
reported, it is the first one.

### Network plotting

The network is exported in a graphml file that is read into Gephi and some
manual tweaking is done there. These are the steps:

* Give the nodes a colour that matches a NEP field [Appearance > Nodes
  (Color) > Attribute > field (then choose colours) > Apply];
* resize nodes so that they reflect the number of followers [Node (Size) >
  attribute > In-Degree (Min size: 20; Max size: 100) > Apply];
* choose a layout [for instance, Layout > OpenOrd (Liquid: 30%; Expansion:
  20%; Cooldown: 20%; Crunch: 20%; Simmer: 10%; Edge Cut: 0.9; Num
  Iterations: 1000; Fixed time=0.2; Random seed: 123) > Run]
* adjust overlap nodes [Layout > Noverlap > Run]
* Save it [File > Export > Sigma.js template]

The last step requires you have the *Sigmajs Exporter* plugin:
<https://marketplace.gephi.org/plugin/sigmajs-exporter/>

In the Sigma.js Export dialog, the following items were filled:

* Features: "Include search?" and "Group edges by direction?"
* Hover behavior: Dim;
* Group selector: field;
* Image attribute: pic;
* Description, long description, etc.

Finally, some tweaks were done for a better visualization.

### Where is the data?

Initially I thought about sharing the data from Twitter and RePEC, but I
reconsidered this idea because I don't feel confortable in sharing personal
information (even if it is publicily available!). You'll need to use the
script to download the data. It will require some time, as Twitter has some
limits on the numbers of API queries.

