library('httr')
library('xml2')
library('magrittr')

page <- 'https://ideas.repec.org/top/top.women.html'

setwd('R:/')

source('keys.txt')

#### OAuth key and secret #####
twitter_token <-
	oauth_app('twitter', key = oauth['key'], secret = oauth['secret']) %>%
	oauth1.0_token(oauth_endpoints('twitter'), .)

GET_content <- function(x) {
	return(content(GET(x, config(token = twitter_token))))
}

all_in_page <- read_html(page) %>%
	xml_find_all('//a[starts-with(@href, "/e/") or starts-with(@href ,"/f/")]/text()') %>%
	as.character %>%
	gsub('  *', ' ', .) %>%
	iconv(from='utf8', to='utf8') %>%
	trimws

twitter_users_on_ideas <- 'https://ideas.repec.org/i/etwitter.html' %>%
	read_html %>%
	xml_find_all('//table/tr/td/a/text()') %>%
	as.character %>%
	gsub('  *', ' ', .) %>%
	iconv(from='utf8', to='utf8') %>%
	trimws %>%
	sub('([^,]+),  *(.*)', '\\2 \\1', .)

on_twitter <- all_in_page %in% twitter_users_on_ideas

cat('There are', sum(on_twitter), 'users on Twitter out of',
	length(all_in_page), 'in\n', page, '\nThey are:')

all_in_page[on_twitter]

