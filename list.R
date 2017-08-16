# List of RePEc economists on Twitter:
# https://ideas.repec.org/i/etwitter.html
# https://twitter.com/chrMongeau/lists/repec-twitter

library('httr')
library('xml2')
library('magrittr')

setwd('R:/')

root <- 'https://api.twitter.com/1.1/lists/'
owner <- 'chrMongeau'
reply_status <- 706873909484851200 # update in_reply_to_status_id

source('keys.txt')

# TODO: error checking on all requests.

error <- FALSE

#### OAuth key and secret #####
twitter_token <-
	oauth_app('twitter', key = oauth['key'], secret = oauth['secret']) %>%
	oauth1.0_token(oauth_endpoints('twitter'), .)

nicks_from_page <- function(u) {
	Sys.sleep(2) # be nice

	html_page <- try(read_html(u))

	if ( 'try-error' %in% class(html_page) ) {
		error <- TRUE
		stop('############ NO INTERNET ##############')
	} else {
		html_page %>%
			xml_find_all('//a[@class="twitter-follow-button"]') %>%
			xml_attr('href') %>%
			sub('.*twitter.com/', '', .)
	}
}

add_to_list <- function(user, slug = NA) {
	paste0(root,
		'members/create.json',
		'?slug=', slug,
		'&owner_screen_name=', owner,
		'&screen_name=', user) %>%
	POST(config(token = twitter_token))
}

list_info <- paste0(root,
		'show.json',
		'?slug=repec-twitter',
		'&owner_screen_name=', owner) %>%
	GET(config(token = twitter_token)) %>%
	content

last_update <- sub('.*(.{10})\\)$', '\\1', list_info$description)

members_raw <- paste0(root,
		'members.json',
		'?slug=repec-twitter',
		'&owner_screen_name=', owner,
		'&count=5000',
		'&skip_status=1') %>%
	GET(config(token = twitter_token))

members <- sapply(content(members_raw)$users, '[[', 'screen_name')

link2users <- 'https://ideas.repec.org/i/etwitter.html' %>%
	read_html %>%
	xml_find_all('//table/tr/td/a') %>%
	sub('<a href="([^\\"]+)\\".*', 'https://ideas.repec.org\\1', .)

N <- length(link2users)
repec_all <- vector(length=N)
for ( i in 1:N ) {
	nick <- nicks_from_page(link2users[i])
	repec_all[i] <- ifelse(length(nick) > 0, nick, NA)
	print(paste(i, N, sep='/'))
	flush.console()
}

if ( error == FALSE ) {
	to_add <- repec_all[!tolower(repec_all) %in% tolower(members) & !is.na(repec_all)]

	add_nick <- sapply(to_add, add_to_list, slug = 'repec-twitter')

	## Remove user from list
	#paste0(root,
	#	'members/destroy.json',
	#	'?slug=', slug,
	#	'&owner_screen_name=', owner,
	#	'&screen_name=', 'SomeUser') %>%
	#	POST(config(token = twitter_token))

	new_list_info <- paste0(root,
			'show.json',
			'?slug=repec-twitter',
			'&owner_screen_name=', owner) %>%
		GET(config(token = twitter_token)) %>%
		content

	new_count <- new_list_info$member_count

	desc <- paste0('Unofficial list of economists on RePEc - ',
			'https://ideas.repec.org/i/etwitter.html') %>%
		url_escape

	# url_encode doesn't encode parentheses
	desc <- paste0(desc, '%20%28as%20on%20', Sys.Date(), '%29')

	update_list <- paste0(root,
			'update.json',
			'?slug=repec-twitter',
			'&owner_screen_name=', owner,
			'&description=', desc) %>%
		POST(config(token = twitter_token))
	#content(update_list)

	updates <- paste(length(to_add), "#RePEc #economists have joined",
			"https://ideas.repec.org/i/etwitter.html from", last_update, 'to',
			Sys.Date(), 'Now', new_count,
			'in list: https://twitter.com/chrMongeau/lists/repec-twitter') %>%
		url_escape

	reply_update <-	paste0('https://api.twitter.com/1.1/statuses/update.json',
			'?status=', updates,
			'&in_reply_to_status_id=', reply_status) %>%
		POST(config(token = twitter_token))
} else {
	print('Some error occurred.')
}

NEP_fields <- c(
    'ACC', 'AFR', 'AGE', 'AGR', 'ARA', 'BAN', 'BEC', 'BIG', 'CBA', 'CBE',
    'CDM', 'CFN', 'CIS', 'CMP', 'CNA', 'COM', 'CSE', 'CTA', 'CUL', 'CWA',
    'DCM', 'DEM', 'DES', 'DEV', 'DGE', 'ECM', 'EDU', 'EEC', 'EFF', 'ENE',
    'ENT', 'ENV', 'ETS', 'EUR', 'EVO', 'EXP', 'FDG', 'FIN', 'FLE', 'FMK',
    'FOR', 'GEN', 'GEO', 'GER', 'GRO', 'GTH', 'HAP', 'HEA', 'HIS', 'HME',
    'HPE', 'HRM', 'IAS', 'ICT', 'IFN', 'IND', 'INO', 'INT', 'IPR', 'IUE',
    'KNM', 'LAB', 'LAM', 'LAW', 'LMA', 'LTV', 'MAC', 'MFD', 'MIC', 'MIG',
    'MKT', 'MON', 'MST', 'NET', 'NEU', 'NPS', 'OPM', 'ORE', 'PAY', 'PBE',
    'PKE', 'POL', 'PPM', 'PUB', 'REG', 'RES', 'RMG', 'SBM', 'SEA', 'SOC',
    'SOG', 'SPO', 'TID', 'TRA', 'TRE', 'TUR', 'UPT', 'URE'
  )

NEP_list <- vector("list", length(NEP_fields))

names(NEP_list) <- NEP_fields

for (i in NEP_fields) {
	current <- paste(i, '->', match(i, NEP_fields), '/', length(NEP_fields))

	NEP_list[[i]] <- list()

	NEP_list[[i]]$link2users <-
		paste0('https://ideas.repec.org/i/e', tolower(i), '.html') %>%
			read_html %>%
			xml_find_all('//table/tr/td/a') %>%
			sub('<a href="([^\\"]+)\\".*', 'https://ideas.repec.org\\1', .)

	in_field <- repec_all[link2users %in% intersect(link2users, NEP_list[[i]]$link2users)]

	if (length(in_field) > 0) {
		NEP_list[[i]]$members_raw <- paste0(root,
			'members.json',
			'?slug=repec-twitter-', tolower(i),
			'&owner_screen_name=', owner,
			'&count=5000',
			'&skip_status=1') %>%
		GET(config(token = twitter_token))

		NEP_list[[i]]$members <- sapply(content(NEP_list[[i]]$members_raw)$users, '[[', 'screen_name')

		NEP_list[[i]]$to_add <- setdiff(tolower(in_field), tolower(NEP_list[[i]]$members))

		if (length(NEP_list[[i]]$to_add) > 0) {
			new_users <- paste('New users:', length(NEP_list[[i]]$to_add))
			NEP_list[[i]]$add_nick <- lapply(NEP_list[[i]]$to_add, add_to_list, slug = paste0('repec-twitter-', tolower(i)))

			NEP_list[[i]]$call_status <- sapply(NEP_list[[i]]$add_nick, function(x) x$status)

			added_users <- paste('Added:', sum(NEP_list[[i]]$call_status == 200))
		} else {
			new_users <- 'No new users'
			added_users <- ''
			NEP_list[[i]]$add_nick <- NULL
		}
	} else {
		NEP_list[[i]]$members_raw <- NULL
		NEP_list[[i]]$members <- NULL
		NEP_list[[i]]$to_add <- NULL
		NEP_list[[i]]$add_nick <- NULL
		NEP_list[[i]]$call_status <- NULL
	}

	print(paste(current, new_users, added_users))
	flush.console()
}

