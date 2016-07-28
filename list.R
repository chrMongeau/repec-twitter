# List of RePEc economists on Twitter:
# https://ideas.repec.org/i/etwitter.html
# https://twitter.com/chrMongeau/lists/repec-twitter

library('httr')
library('xml2')
library('magrittr')

setwd('R:/')

root <- 'https://api.twitter.com/1.1/lists/'
slug <- 'repec-twitter'
owner <- 'chrMongeau'
reply_status <- 706873909484851200 # update in_reply_to_status_id

source('keys.txt')

#### OAuth key and secret #####
twitter_token <-
	oauth_app('twitter', key = oauth['key'], secret = oauth['secret']) %>%
	oauth1.0_token(oauth_endpoints('twitter'), .)

nicks_from_page <- function(u) {
	Sys.sleep(2) # be nice

	read_html(u) %>%
		xml_find_all('//a[@class="twitter-follow-button"]') %>%
		xml_attr('href') %>%
		sub('.*twitter.com/', '', .)
}

add_to_list <- function(user) {
	paste0(root,
		'members/create.json',
		'?slug=', slug,
		'&owner_screen_name=', owner,
		'&screen_name=', user) %>%
	POST(config(token = twitter_token))
}


list_info <- paste0(root,
		'show.json',
		'?slug=', slug,
		'&owner_screen_name=', owner) %>%
	GET(config(token = twitter_token)) %>%
	content

last_update <- sub('.*(.{10})\\)$', '\\1', list_info$description)

members_raw <- paste0(root,
		'members.json',
		'?slug=', slug,
		'&owner_screen_name=', owner,
		'&count=5000',
		'&skip_status=1') %>%
	GET(config(token = twitter_token))

members <- sapply(content(members_raw)$users, '[[', 'screen_name')

link2users <- 'https://ideas.repec.org/i/etwitter.html' %>%
	read_html %>%
	xml_find_all('//table/tr/td/a') %>%
	sub('<a href="([^\\"]+)\\".*', 'https://ideas.repec.org\\1', .)

# TODO: handle error in the event of no internet connection
#       (the scripts fails, but continues without saying so)
N <- length(link2users)
repec_all <- vector(length=N)
for ( i in 1:N ) {
	nick <- nicks_from_page(link2users[i])
	repec_all[i] <- ifelse(length(nick) > 0, nick, NA)
	print(paste(i, N, sep='/'))
	flush.console()
}

to_add <- repec_all[!tolower(repec_all) %in% tolower(members) & !is.na(repec_all)]

add_nick <- sapply(to_add, add_to_list)

## Remove user from list
#paste0(root,
#	'members/destroy.json',
#	'?slug=', slug,
#	'&owner_screen_name=', owner,
#	'&screen_name=', 'SomeUser') %>%
#	POST(config(token = twitter_token))

desc <- paste0('Unofficial list of economists on RePEc - ',
		'https://ideas.repec.org/i/etwitter.html') %>%
	url_escape

# url_encode doesn't encode parentheses
desc <- paste0(desc, '%20%28as%20on%20', Sys.Date(), '%29')

update_list <- paste0(root,
		'update.json',
		'?slug=', slug,
		'&owner_screen_name=', owner,
		'&description=', desc) %>%
	POST(config(token = twitter_token))
#content(update_list)

updates <- paste(length(to_add), "#RePEc #economists have joined",
		"https://ideas.repec.org/i/etwitter.html from", last_update, 'to',
		Sys.Date(), 'List: https://twitter.com/chrMongeau/lists/repec-twitter') %>%
	url_escape

reply_update <-	paste0('https://api.twitter.com/1.1/statuses/update.json',
		'?status=', updates,
		'&in_reply_to_status_id=', reply_status) %>%
	POST(config(token = twitter_token))

