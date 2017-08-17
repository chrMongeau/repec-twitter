# Name of the graph that should be imported into Gephi
to_gephi <- 'network.graphml'

setwd('R:/')

library('httr')
library('xml2')
library('igraph')
library('dplyr')
library('magrittr')
library('proxy')

source('keys.txt')

api <- 'https://api.twitter.com/1.1/'

twitter_token <-
	oauth_app('twitter', key = oauth['key'], secret = oauth['secret']) %>%
	oauth1.0_token(oauth_endpoints('twitter'), .)

GET_content <- function(x) {
	return(content(GET(x, config(token = twitter_token))))
}

asDataFrame <- function(x) {
	x %<>%
		as.data.frame %>%
		t %>%
		as.data.frame(stringsAsFactors=FALSE)

	rownames(x) <- NULL

	return(x)
}

epoch <- function(x) {
	return(as.numeric(format(x, format='%s')))
}

tw_sleep <- function() {
	rate_limit <-
		paste0(api, 'application/rate_limit_status.json?resources=friends') %>%
		GET_content

	x <- rate_limit$resources$friends$`/friends/ids`$reset
	to_sleep <- x - epoch(Sys.time()) + 10

	print(paste('Sleeping until', format(Sys.time()+to_sleep, '%H:%M:%S')))
	flush.console()
	Sys.sleep(to_sleep)
}

members_raw <-
	paste0(api, 'lists/members.json',
		'?slug=repec-twitter&owner_screen_name=chrMongeau',
		'&count=5000&skip_status=1') %>%
	GET(config(token = twitter_token))

members <- data.frame(
			nicks=sapply(content(members_raw)$users, '[[', 'screen_name'),
			ids=sapply(content(members_raw)$users, '[[', 'id_str'),
			location=NA,
			protected=NA,
			followers=NA,
			friends=NA,
			joined=NA,
			favs=NA,
			geo=NA,
			statuses=NA,
			lang=NA,
			description=NA,
			pic=NA,
			stringsAsFactors=FALSE)

N <- nrow(members)

adj <- matrix(0, nrow=N, ncol=N)
colnames(adj) <- rownames(adj) <- members$ids

# TODO: handle no internet ("Couldn't connect to server")
for ( i in 1:N ) {
	print(paste(i, N, sep='/')); flush.console()
	fuser <- members$ids[i]
	cursor <- -1 # friends_ids$next_cursor_str

	friends_ids <-
		paste0(api, 'friends/ids.json?', 'cursor=', cursor,
		   '&user_id=', fuser, '&count=5000&stringify_ids=true') %>%
		GET_content

	# Won't test for errors
	twinfo <-
		paste0(api, 'users/show.json?user_id=', fuser) %>%
		GET_content

	other_error <- FALSE

	if ( !is.null(friends_ids$errors) ) {
		if ( friends_ids$errors[[1]]$code == 88 ) {
			print('API ERROR: LIMIT') ; flush.console()

			while ( !is.null(friends_ids$errors) ) {

				tw_sleep()

				friends_ids <-
					paste0(api, 'friends/ids.json?', 'cursor=', cursor,
					   '&user_id=', fuser, '&count=5000&stringify_ids=true') %>%
					GET_content
			}
		} else {
			print('API ERROR: OTHER') ; flush.console()
			other_error <- TRUE
		}
	}

	if ( other_error ) {
		adj[fuser,] <- NA
	} else {
		adj[fuser,] <- rownames(adj) %in% as.character(unlist(friends_ids$ids))
		members[i,c(
			'location',
			'protected',
			'followers',
			'friends',
			'joined',
			'favs',
			'geo',
			'statuses',
			'lang',
			'description',
			'pic')] <-
				twinfo[c(
					'location',
					'protected',
					'followers_count',
					'friends_count',
					'created_at',
					'favourites_count',
					'geo_enabled',
					'statuses_count',
					'lang',
					'description',
					'profile_image_url')]
	}
}

members$perday <- round(members$statuses /
	((epoch(Sys.time()) - epoch(as.Date(members$joined, format='%a %b %d %H:%M:%S %z %Y')))/(60*60*24)))

members_orig <- members

##### INFO from IDEAS

NEP_fields <-
	'https://ideas.repec.org/n/' %>%
	read_html %>%
	xml_find_all('//ul/li/b') %>%
	sub('<b><a href="nep-.../">NEP-(...)</a> *([^<]+) *</b>', '\\1|\\2', .) %>%
	strsplit('\\|') %>%
	asDataFrame

rownames(NEP_fields) <- NULL
colnames(NEP_fields) <- c('field', 'name')

NEP_fields$name %<>% sub('&amp;', '&', .)

pubs_from_page <- function(pag, type=NA) {
	pubs <-
		pag %>%
		xml_find_all(paste0('//div[@id="', type, '-body"]/ol/li')) %>%
		length

	return(pubs)
}

# TODO: add EDIRC Handle
affiliation_from_page <- function(pag) {
	instit <-
		pag %>%
		xml_find_all('//div[@id="affiliation-body"]//h4') %>%
		sub('.*"collapsed">(.*)<\\/a>.*', '\\1', .) %>%
		gsub('<br\\/?>', ', ', .) %>%
		trimws

	if ( length(instit) == 0 ) {
		return(data.frame(instit=NA, location=NA, percent=NA, n=NA))
	} else if ( length(instit) == 1 ) {
		instit <- data.frame(V1 = 100, V2 = instit, stringsAsFactors = FALSE)
	} else {
		instit %<>%
		sub('%)', '%#)', .) %>%
		strsplit('#) ') %>%
		asDataFrame

		# If no percentage is reported
		if ( ncol(instit) ==1 ) {
			instit <- data.frame(V1=paste0('(', 100/nrow(instit)), V2=instit$V1)
		}

		instit$V1 %<>%
			sub('\\(', '', .) %>%
			sub('%', '', .) %>%
			as.numeric
	}

	locat <-
		pag %>%
		xml_find_all('//span[@class="locationlabel"]/text()') %>%
		trimws

	return(data.frame(instit = instit$V2[1], location = locat[1],
					  percent = instit$V1[1], n = length(instit$V1),
					  stringsAsFactors = FALSE))
}

fields_from_page <- function(pag) {
	fields <-
		pag %>%
		xml_find_all('//div[@id="author-nep-fields"]//li') %>%
		sub('<li><a href="/n/nep-.../">NEP-...</a>(: .* \\([0-9]+)\\) <a href=.*', '\\1', .) %>%
		gsub('<.?b>', '', .) %>%
		strsplit('\\(') %>%
		asDataFrame

	if ( nrow(fields) == 0 | ncol(fields) < 2 ) {
		return(list(fields=paste(rep(NA, nrow(NEP_fields)), collapse='#'), n=0))
	} else {

		fields[,1] %<>%
			sub('^: ', '', .) %>%
			sub('  *$', '', .)

		fields[,2] %<>%
			as.numeric

		fields$field <-
			pag %>%
			xml_find_all('//div[@id="author-nep-fields"]//li/a[1]/text()') %>%
			as.character %>%
			sub('NEP-', '', .)

		jfields <- left_join(NEP_fields, fields, by='field')$V2

		nfields <- sum(!is.na(jfields))

		fields_str <- paste(jfields, collapse='#')
		fields_str <- gsub('NA', 0, fields_str)

		return(list(fields=fields_str, n=nfields))
	}
}

name_from_page <- function(pag) {
	name <-
		pag %>%
		xml_find_all('//h1/text()') %>%
		iconv(from='utf8', to='utf8') %>%
		trimws %>%
		sub('  *', ' ', .)

	return(ifelse(length(name)>0, name, NA))
}

nick_from_page <- function(pag) {
	nick <-
		pag %>%
		xml_find_all('//a[@class="twitter-follow-button"]') %>%
		xml_attr('href') %>%
		sub('.*twitter.com/', '', .)

	return(ifelse(length(nick) > 0, nick, NA))
}

# Revealed Field Advantage (from "Revealed Technological Advantage";
# Proudman and Redding, 1998)
RFA_fun <- function(x) {
	a <- apply(x, 2, sum, na.rm=TRUE)
	b <- t(t(x)/a)
	b[is.nan(b)] <- 0

	X <- b/(1/ncol(x)*apply(b, 1, sum))
	X[is.nan(X)] <- 0

	return(X)
}

link2users <-
	'https://ideas.repec.org/i/etwitter.html' %>%
	read_html %>%
	xml_find_all('//table/tr/td/a') %>%
	sub('<a href="([^\\"]+)\\".*', 'https://ideas.repec.org\\1', .)

users <- data.frame(nick='',
					name='',
					link=link2users,
					articles=-1,
					papers=-1,
					software=-1,
					fields='',
					nfields='',
					affil_inst='',
					affil_loc='',
					affil_perc='',
					affil_n='',
					stringsAsFactors=FALSE)

N <- nrow(users)
for ( i in 1:N ) {
	print(paste(i, N, sep='/')) ; flush.console()

	pag <- read_html(link2users[i])

	users$nick[i] <- nick_from_page(pag)

	users$name[i] <- name_from_page(pag)

	users[i, c('fields', 'nfields')] <-
		fields_from_page(pag)[c('fields', 'n')]

	users[i, c('affil_inst', 'affil_loc', 'affil_perc', 'affil_n')] <-
		affiliation_from_page(pag)[c('instit', 'location', 'percent', 'n')]

	users$articles[i] <- pubs_from_page(pag, type='articles')
	users$papers[i] <- pubs_from_page(pag, type='papers')
	users$software[i] <- pubs_from_page(pag, type='software')

	Sys.sleep(0.2) # be nice
}

# Backup
users_orig <- users

# Remove users with no Twitter info
users <- users[!(tolower(users$nick) %in% tolower(members$nick[is.na(members$followers)])),]

users$name_nick <- paste0(users$name, ' (@', users$nick, ')')

users_fields <-
	users$fields %>%
	strsplit('#') %>%
	lapply(as.numeric) %>%
	as.data.frame %>%
	t

colnames(users_fields) <- NEP_fields$field
rownames(users_fields) <- users$name_nick

# Main field (if ties, random assignment)
users$field <-
	colnames(users_fields)[apply(users_fields, 1, function(x) max.col(t(x)))]

users$field_desc <-
	NEP_fields$name[match(users$field, NEP_fields$field)]

# Specialisation
RFA <- RFA_fun(users_fields)

users$special <-
	colnames(RFA)[apply(RFA, 1, function(x) max.col(t(x)))]

users$special_desc <-
	NEP_fields$name[match(users$special, NEP_fields$field)]

# Aggregate fields with low frequencies into XYZ
users$field1 <-
	users$field %>%
	sapply(function(x) ifelse(table(users$field)[x] >= 5, x, 'XYZ'))



NEP_alt <- read.csv('NEP.csv', stringsAsFactors=FALSE)

users$field1_desc <- NEP_alt$group[match(users$field, NEP_alt$field)]



# If user changed its username, it won't be matched. Sorry.
no_match <- members$nicks[! tolower(members$nicks) %in% tolower(users$nick)]
if ( length(no_match) > 0 ) {
	x <- ! members$nicks %in% no_match
	members <- members[x,]
}

matched <- match(tolower(members$nicks), tolower(users$nick))

users <- users[matched,]
users_fields <- users_fields[matched,]

# Backup
adj_orig <- adj

adj <- adj[colnames(adj) %in% members$ids, colnames(adj) %in% members$ids]

#rownames(adj) <- colnames(adj) <- members$nicks
rownames(adj) <- colnames(adj) <- users$name_nick

## Weighted by % of people followed:
#adj <- adj/apply(adj, 1, sum)
#adj[is.nan(adj)] <- 0

# Remove NEP field "ALL"
users_fields %<>% subset(select=-ALL)

# Weighted by similarity of NEP fields
simNEP <-
	users_fields %>%
	simil(method='cosine') %>%
	as.matrix(diag=0)

simNEP[is.na(simNEP)] <- 0

adj_sim <- adj * simNEP

# Remove isolates
x <- apply(adj_sim, 1, sum, na.rm=TRUE) != 0 | apply(adj_sim, 2, sum, na.rm=TRUE) != 0
adj_sim <- adj_sim[x, x]

users <- users[x,]
users_fields <- users_fields[x,]

net <-
	adj_sim %>%
	graph_from_adjacency_matrix(mode='directed', weighted=TRUE) %>%
	set_vertex_attr('link', value = users$link) %>%
	set_vertex_attr('field', value = users$field) %>%
	#set_vertex_attr('field1', value = users$field1) %>%
	set_vertex_attr('field1_desc', value = users$field1_desc) %>%
	set_vertex_attr('papers', value = users$papers) %>%
	set_vertex_attr('articles', value = users$articles) %>%
	set_vertex_attr('software', value = users$software) %>%
	set_vertex_attr('field_name', value = users$field_name) %>%
	set_vertex_attr('centrality', value = page.rank(.)$vector) %>%
	set_vertex_attr('centrality_rank', value = rank(page.rank(.)$vector)) %>%
	set_vertex_attr('pic', value = members$pic[match(tolower(users$nick),
													 tolower(members$nicks))])


write_graph(net, to_gephi, format='graphml')


# Hack: change generic node id (e.g., n11) name to username_nick
a <- readLines(to_gephi)

for ( i in 0:(nrow(adj_sim)-1) ) {
		a <- gsub(paste0('\\bn', i, '\\b'), users$name_nick[i+1], a)
}

cat(a, file=to_gephi, sep='\n')


# https://twitter.com/intent/user?user_id=2320913786
