library('httr')
library('xml2')
library('magrittr')


epoch <- function(x) {
	return(as.numeric(format(x, format='%s')))
}

#### OAuth key and secret #####
myapp <- oauth_app('twitter',
	key = 'iwIqHoBozVusJ8jucAeXAtrML',
	secret = 'qy0ZWvFXTTEqjlMZW03SRGFdKotn3rSiyh7dhz3jtVIdO7gEj4'
)

twitter_token <- oauth1.0_token(oauth_endpoints('twitter'), myapp)


GET_content <- function(x) {
	return(content(GET(x, config(token = twitter_token))))
}



TOP5 <- 'https://ideas.repec.org/top/top.person.all10.html' %>%
	read_html %>%
	xml_find_all('//table[@class="shorttop"]//td/a[@href]/text()') %>%
	as.character %>%
	gsub('  *', ' ', .) %>%
	iconv(from='utf8', to='utf8') %>%
	trimws

TOP <- 'https://ideas.repec.org/top/top.person.all10.html' %>%
	read_html %>%
	xml_find_all('//a[contains(@href, "/e/") or contains(@href ,"/f/")]/text()') %>%
	as.character %>%
	gsub('  *', ' ', .) %>%
	iconv(from='utf8', to='utf8') %>%
	trimws

rate_limit <- 'https://api.twitter.com/1.1/application/rate_limit_status.json?resources=users' %>%
	GET_content

rate_limit$resources$users$`/users/search`$remaining



x <- rate_limit$resources$users$`/users/search`$reset
to_sleep <- x - epoch(Sys.time()) + 10
print(paste('Sleeping until', format(Sys.time()+to_sleep, '%H:%M:%S')))

# TODO: common names requires iterating through various pages
# TODO: remove "Jr." etc.

users <- list()
for ( i in 2300:2478 ) {
	print(i) ; flush.console()

	user <- TOP[i] %>%
		gsub(' ', '%20', .)

	a <- paste0('https://api.twitter.com/1.1/users/search.json?q=',
		user, '&page=1&count=20') %>%
		GET_content

	if ( !is.null(a$errors) ) {
		if ( a$errors[[1]]$code == 88 ) stop()
	}

	found <- grep('econ', sapply(a, '[[', 'description'), ignore.case=TRUE)

	if ( length(found) > 0 ) {
		users[[i]] <- sapply(found, function(x) a[[x]]$screen_name)
		print(-1) ; flush.console()
	} else {
		 # Try removing middle name initials
		user <- TOP[i] %>%
			sub(' [A-Z]\\. ', ' ', .) %>%
			gsub(' ', '%20', .)
		
			found <- grep('econ', sapply(a, '[[', 'description'), ignore.case=TRUE)

		if ( length(found) > 0 ) {
			users[[i]] <- sapply(found, function(x) a[[x]]$screen_name)
			print(-2) ; flush.console()
		} else {
			# Search in last tweet
			found <- grep('econ', sapply(a, function(x) x[['status']][['text']]), ignore.case=TRUE)
			
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


sum(!unlist(sapply(users, is.na))) / length(unlist(sapply(users, is.na)))*100


