library('httr')
library('xml2')
library('magrittr')

page <- 'https://ideas.repec.org/top/top.women.html'

setwd('R:/')

source('keys.txt')

epoch <- function(x) {
	return(as.numeric(format(x, format='%s')))
}

#### OAuth key and secret #####
twitter_token <-
	oauth_app('twitter', key = oauth['key'], secret = oauth['secret']) %>%
	oauth1.0_token(oauth_endpoints('twitter'), .)

GET_content <- function(x) {
	return(content(GET(x, config(token = twitter_token))))
}

tw_sleep <- function() {
	rate_limit <-
		paste0('https://api.twitter.com/1.1/', 'application/rate_limit_status.json?resources=search') %>%
		GET_content

	x <- rate_limit$resources$search$`/search/tweets`$reset
	to_sleep <- x - epoch(Sys.time()) + 10

	print(paste('Sleeping until', format(Sys.time()+to_sleep, '%H:%M:%S')))
	flush.console()
	Sys.sleep(to_sleep)
}

# TOP5 <- 'https://ideas.repec.org/top/top.person.all10.html' %>%
# 	read_html %>%
# 	xml_find_all('//table[@class="shorttop"]//td/a[@href]/text()') %>%
# 	as.character %>%
# 	gsub('  *', ' ', .) %>%
# 	iconv(from='utf8', to='utf8') %>%
# 	trimws
# 
# TOP5 <- TOP5[!grepl('[^\\w\\.\\)]$', TOP5, perl=T)]

TOP <- page %>%
	read_html %>%
	xml_find_all('//a[starts-with(@href, "/e/") or starts-with(@href ,"/f/")]/text()') %>%
	as.character %>%
	gsub('  *', ' ', .) %>%
	iconv(from='utf8', to='utf8') %>%
	trimws

TOP <- TOP[!grepl('[^\\w\\.\\)]$', TOP, perl=TRUE)]

# TODO: common names requires iterating through various pages
# TODO: remove "Jr." etc.

# XXX: amartya sen -> https://twitter.com/NotAmartyaSen (parody)

users <- list()
#for ( i in seq_along(TOP5) ) {
for ( i in seq_along(TOP) ) {
	print(i) ; flush.console()

	other_error <- FALSE

	user <- TOP[i] %>%
		gsub(' ', '%20', .)

	a <- paste0('https://api.twitter.com/1.1/users/search.json?q=',
		user, '&page=1&count=20') %>%
		GET_content

	if ( !is.null(a$errors) ) {
		if ( a$errors[[1]]$code == 88 ) {
			print('API ERROR: LIMIT') ; flush.console()

			while ( !is.null(a$errors) ) {

				tw_sleep()

				a <- paste0('https://api.twitter.com/1.1/users/search.json?q=',
					user, '&page=1&count=20') %>%
					GET_content
			}
		} else {
			print('API ERROR: OTHER') ; flush.console()
			other_error <- TRUE
		}
	}

	if ( other_error ) {
		users[[i]] <- NA
	} else {
		found <- grep('\\becon', sapply(a, '[[', 'description'), ignore.case=TRUE)

		if ( length(found) > 0 ) {
			users[[i]] <- sapply(found, function(x) a[[x]]$screen_name)
			print(-1) ; flush.console()
		} else {
			 # Try removing middle name initials
			user <- TOP[i] %>%
				sub(' [A-Z]\\. ', ' ', .) %>%
				gsub(' ', '%20', .)
			
				found <- grep('\\becon', sapply(a, '[[', 'description'), ignore.case=TRUE)

			if ( length(found) > 0 ) {
				users[[i]] <- sapply(found, function(x) a[[x]]$screen_name)
				print(-2) ; flush.console()
			} else {
				# Search in last tweet
				found <- grep('\\becon', sapply(a, function(x) x[['status']][['text']]), ignore.case=TRUE)
				
				if ( length(found) > 0 ) {
					users[[i]] <- sapply(found, function(x) a[[x]]$screen_name)
					print(-3) ; flush.console()
				} else {
					# Add search in tweets
					users[[i]] <- NA
				}
			}
		}
	}
}


sum(!unlist(sapply(users, is.na))) / length(unlist(sapply(users, is.na)))*100


