library('httr')
library('xml2')
library('magrittr')
library('stringi')


#### OAuth key and secret #####
myapp <- oauth_app('twitter',
	key = 'iwIqHoBozVusJ8jucAeXAtrML',
	secret = 'qy0ZWvFXTTEqjlMZW03SRGFdKotn3rSiyh7dhz3jtVIdO7gEj4'
)

twitter_token <- oauth1.0_token(oauth_endpoints('twitter'), myapp)


GET_content <- function(x) {
	return(content(GET(x, config(token = twitter_token))))
}


main <- 'https://ideas.repec.org/e/pgi18.html' %>%
	read_html %>%
	xml_find_all('//div[@id="author-articles"]//ol/li') %>%
	as.character %>%
	iconv(from='utf8', to='utf8') %>%
	sub('\\n.*', '', .) %>%
	sub('<li class="[^"]+">', '', .) %>%
	gsub(' &amp; ', '|', .) %>%
	sub(',  *\\d\\d\\d\\d\\.?$', '', .) %>%
	trimws %>%
	strsplit('\\|') %>%
	unlist %>%
	sub('([^,]+),  *(.*)', '\\2 \\1', .) %>%
	gsub('([A-Z][a-z]?)\\b\\.? ?', '', .) %>% # XXX (C. | C.A. | Ch.)
	stri_trans_general('latin-ascii') %>%
	table %>%
	as.data.frame(stringsAsFactors=FALSE)


# Removing self
main <- main[-which.max(main[,2]),]

# Removing authors without name
main <- main[unlist(lapply(strsplit(main[,1], ' '), length)) > 1,]

d <- stringdistmatrix(main[,1]) %>%
	as.matrix

diag(d) <- NA

d[d>3] <- NA

w <- apply(d, 1, function(x) ifelse(sum(!is.na(x))>0, which.min(x), NA))
