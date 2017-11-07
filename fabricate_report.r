library(stringi)
library(plyr)
library(reshape2)
library(dplyr)
library(lazyeval)
library(ggplot2)
library(ggthemes)
library(cetcolor)
library(scales)
library(knitr)
#source('/Users/mcandocia/Documents/sweatex.r')

#setwd('/Users/mcandocia/Downloads')
#setwd('/ntfsl/Dropbox/Grassroots Research Group')
setwd('/Users/mcandocia/Dropbox/Grassroots Research Group')
sweatex <- function(filename,extension='Rnw',command='pdflatex',silent=FALSE,preview=FALSE)
{
  if (command=='latex') command='simpdftex latex --maxpfb'
  extension<-paste('.',extension,sep='')
  path=options('latexcmd')[[1]]
  path=substr(path,start=1,stop=nchar(path)-5)
  Sweave(paste(filename,extension,sep=''))
  system(paste(path,command,' ',filename,sep=''),intern=silent)
  # if (command=='latex')
  # {
  # system(paste(path,'dvipdfm',' ',filename,sep=''))
  # }
  
  if (preview)
  {
    system(paste(options('pdfviewer')[[1]],' ',filename,'.pdf',sep=''))
  }
}

#setwd('/Users/mcandocia/Dropbox/Grassroots Research Group')
df = read.csv('20171002191600-SurveyExport.csv')


all_are_same <- function(x){
  length(unique(x))==1
}

is_not_empty <- function(x){
  !is.na(x) & x != ''
}

#kind of stupid, but reverses strings to find longest common substring from tail end 
#(done this way so indexes all start/end at same values)
find_common_pattern <- function(strings){
  maxlen=max(nchar(strings))
  reversed = stringi::stri_reverse(strings)
  substrings = rep('', length(reversed))
  max_cnter = 1
  while (TRUE){
    substrings=substr(reversed, 1, max_cnter)
    if (!all_are_same(substr(reversed,1, max_cnter+1)))
      break
    max_cnter = max_cnter + 1
  }
  common_substring = stringi::stri_reverse(substrings[1])
  return(common_substring)
}

#id.col is currently unused, just a reminder
expand_column_group <- function(data, colrange, other_pair=NULL, id.col=1,
                                summary_type='yn', manual_question=NULL){
  newdata = data[,c(1,colrange)]
  #drop=FALSE in case there's only 1 other column
  cnames = colnames(newdata[,-1,drop=FALSE])
  if (is.null(manual_question)){
    if (!is.null(other_pair) & FALSE)#causing bugs with duplicate names in the other_pair
      pattern_cnames = c(cnames, names(data)[other_pair])
    else
      pattern_cnames=cnames
    question_string = find_common_pattern(pattern_cnames)
    print(question_string)
    response_string = substr(cnames,1,nchar(cnames)-nchar(question_string))
  }
  else{
    response_string = cnames
    print(manual_question)
  }
  #print(cnames)
  #print(response_string)
  #summary_type - yn= yes/no
  #               ranking = ranked
  #               cat = categorical response
  
  #for categorical, return response
  #for ranked, return non-blank response
  #for yes/no, return response, but ''=='NO', other=='YES'
  if (summary_type=='yn'){
    newdata[,cnames] = plyr::colwise(function(x) ifelse(x %in% c('',NA,'NO'),'NO','YES'))(newdata[,cnames])
  }
  id_colname = names(data)[id.col]
  #rename columns
  names(newdata)[-1] = response_string
  #now make tall
  melted = melt(newdata,id.vars='Response.ID')
  #now filter, if necessary
  if (summary_type=='ranked'){
    melted=melted %>% filter(value != '')
  }
  #append OTHER data if exists
  if (!is.null(other_pair)){
    other_df = data[,c(1, other_pair)]
    if (length(other_pair)==1){
      #just look at all non-empty values
      valid_rows = is_not_empty(data[,other_pair,drop=FALSE] )
      if (sum(valid_rows) > 0){
        subdata = data[valid_rows,c('Response.ID'), drop=FALSE]
        subdata$variable=data[valid_rows,other_pair]
        subdata$value='YES'
        #print(names(subdata))
        #print(names(melted))
        melted = rbind(melted, subdata)
      }
    }
    else{
      #filter out by first column, then use all the rest (should be non-empty)
      valid_rows = is_not_empty(data[,other_pair[1]] )
      if (sum(valid_rows) > 0){
        print(valid_rows)
        subdata = data[valid_rows,c('Response.ID'), drop=FALSE]
        #subdata <<- subdata
        subdata$variable=data[valid_rows,other_pair[2]]
        subdata$value='YES'
        
        #print(names(subdata))
        #print(names(melted))
        melted = rbind(melted, subdata)
      }
    }
  }
  return(melted)
}

#simulations testing
rand_cat <- function(n, n_cats=5)
  return(letters[sample(1:n_cats,n,replace=T)])

rand_yn <- function(n, p=0.5){
  return(c('YES','NO')[(runif(n)>p)+1])
}

rand_yb <- function(n, p=0.5){
  return(c('YES','')[(runif(n)>p)+1])
}

rand_rank <-function(n, n_cats=5, n_ranks=3){
  mat = matrix(nrow=n, ncol=n_cats, NA)
  vals = replicate(n, sample(1:n_cats,n_ranks))
  for (i in 1:n)
    for (rank in 1:n_ranks)
      mat[i, vals[rank, i]] = rank
  return(as.data.frame(mat))
}

rand_cat_blank <- function(n, n_cats=3, p=0.8){
  ifelse(runif(n) > p, rand_cat(n, n_cats), NA)
}

#looks like there's 1 extra for first one...TODO: CHECK
#for parsing, find the common pattern within each group

# 1 - Response.ID
#VALIDATION, POSSIBLY
# 2 - Time started
# 3 - Date submitted
# 4 - Status of survey

clean_varnames <- function(x) {
  x = gsub('\\.\\.','/',x)
  x = gsub('\\.', ' ', x)
  return(x)
}

multiorder <- function(cols){
  #levels x ranks # row x column # table(variable, value)
  ranks = list()
  for (i in 1:ncol(cols)){
    ranks[[i]] = -cols[,i]
  }
  do.call(order, ranks)
}

#used for yes/no data as well as general class/text data; not for ordinal data
#sendtoback is used to indicate variables that are non-responses/hard-coded "other" 
# values that should be pushed back and included regardless
resort_by_frequency <- function(data, cutoff=1, max_fields = 30, mode='yn', 
                                sendtoback=NULL, reverse=FALSE,
                                ...){
  order_trans = ifelse(reverse, rev, identity)
  if (mode=='yn'){
    if (!is.null(sendtoback))
      counts = table(data %>% filter(value=='YES') %>% select(variable))
    else
      counts = table(data %>% filter(!variable %in% sendtoback & value=='YES') %>% select(variable))
    counts = counts[counts >= cutoff]
    sorted_counts = counts[order(-counts)]
    if (length(counts) > max_fields){
      sorted_counts = sorted_counts[1:max_fields]
    }
    sorted_counts = order_trans(sorted_counts)
    new_names = names(sorted_counts)
    if (!is.null(sendtoback))
      new_names = c(new_names,sendtoback)
    data = data %>% 
      mutate(variable = factor(as.character(variable), levels=new_names))
    if (any(is.na(data$variable)))
      data=data %>% filter(!is.na(variable))#data$variable = mapvalues(addNA(data$variable), from=NA, to="Other")
    return(data)
  }
  else if (mode=='ranked'){
    #sort by most in 1st, then 2nd, then 3rd, etc.
    rank_table = table(data$variable, data$value)
    rank_mat = as.matrix(rank_table)
    rank_order = multiorder(rank_mat)
    rank_order = order_trans(rank_order)
    new_names = rownames(rank_table)[rank_order]
    data$variable = factor(as.character(data$variable), levels = new_names)
    
    return(data)
  }
  else{
    if (!is.null(sendtoback))
      counts = table(data$variable)
    else
      counts = table(data %>% filter(!variable %in% sendtoback) %>% select(variable))
    counts = counts[counts >= cutoff]
    sorted_counts = counts[order(-counts)]
    if (length(counts) > max_fields){
      sorted_counts = sorted_counts[1:max_fields]
    }
    sorted_counts = order_trans(sorted_counts)
    new_names = names(sorted_counts)
    if (!is.null(sendtoback))
      new_names = c(new_names,sendtoback)
    data = data %>% 
      mutate(variable = factor(as.character(variable), levels=new_names))
    if (any(is.na(data$variable)))
      data=data %>% filter(!is.na(variable))#data$variable = mapvalues(addNA(data$variable), from=NA, to="Other")
    return(data)
  }
}

samplesize <- function(data){
  return(length(unique(data$Response.ID)))
}

plot_survey_data <- function(data, mode='yn', level_order=NULL, val_level_order=NULL, ...){
  #remove "No resposne/NA values"
  data = data %>% filter(!variable %in% c("No Response", "NA", NA) & !is.na(variable)) 
  
  if ('value' %in% names(data))
   data = data %>% filter(!value %in% c("No Response", "NA", NA) & !is.na(value))
  
  #assign unique number globally so it can be accessed from ggtitle function in parent environment
  n_unique_responses <<- length(unique(data$Response.ID))
  custom_percent <- function(x){
    percent(x/n_unique_responses)
  }
  
  data = data %>% clean_results(mode=mode)
  data$n_unique_responses = n_unique_responses
  if (mode=='yn'){
    data = resort_by_frequency(data, mode='yn', reverse=TRUE, ...)
    ngroups = length(unique(data$variable))
    if (!is.null(level_order)){
      data$variable = factor(data$variable, levels=level_order)
    }
    if (ngroups > 4){
      flip = coord_flip()
      x_axis_switch = NULL#scale_y_reverse()
    }
    else{
      flip = NULL
      x_axis_switch = NULL
    }
    p = (ggplot(data %>% filter(value=='YES'), aes(x=variable)) +
           geom_bar(aes(y=(..count..), fill=(..count..))) + 
           flip + x_axis_switch +
           scale_y_continuous(label=custom_percent, breaks=seq(0, n_unique_responses, length.out=6)))
  }
  else if (mode=='ranked'){
    data = resort_by_frequency(data, mode='ranked', reverse=TRUE)
    ngroups = length(unique(data$variable))
    if (ngroups > 4){
      flip = coord_flip()
      x_axis_switch = NULL#scale_y_reverse(
    }
    else{
      flip = NULL
      x_axis_switch = NULL
    }
    if (!is.null(level_order)){
      data$variable = factor(data$variable, levels=level_order)
    }
    p = (ggplot(data, aes(x=variable, fill=factor(value, levels=max(value):1))) +
           geom_bar() + 
           flip + x_axis_switch + 
           scale_fill_pander('Ranking') +
           scale_y_continuous(label=custom_percent, breaks=seq(0, n_unique_responses, length.out=6)))
    
  }
  else if (mode==''){
    data = resort_by_frequency(data, mode='', reverse=TRUE, ...)
    if (!is.null(val_level_order)){
      data$value = factor(data$value, levels=val_level_order)
      #filter out blank data
      data = data %>% filter(!is.na(value))
    }
    ngroups = length(unique(data$variable))
    if (ngroups > 4){
      flip = coord_flip()
      x_axis_switch = NULL#scale_y_reverse()
    }
    else{
      flip = NULL
      x_axis_switch = NULL
    }
    if (!is.null(level_order)){
      data$variable = factor(data$variable, levels=level_order)
    }
    
    p = (ggplot(data %>% filter(TRUE), aes(x=variable)) +
           geom_bar(aes(y=(..count..), fill=value), position=position_stack(reverse=TRUE)) + 
           flip + x_axis_switch +
           scale_y_continuous(label=custom_percent, breaks=seq(0, n_unique_responses, length.out=6)))
  }
  if (mode != 'ranked'){
    if (mode=='yn'){
      fill_func = scale_fill_gradientn('', colors=cet_pal(5,'inferno'))
      fill_guide = guides(fill=FALSE)
    }
    else{
      fill_func = scale_fill_pander()
      fill_guide=NULL
    }
    ylabel = ylab('')
  }
  else{
    fill_func = theme(legend.text=element_text(size=rel(3)))
    fill_guide = NULL
    ylabel = ylab('')
  }
  return(p + ylabel + theme(text=element_text(size=rel(3.5)),
                            plot.title=element_text(size=rel(3.5)),
                            plot.subtitle=element_text(size=rel(2.5))) +
           fill_func + fill_guide)
}

clean_results <- function(data, ...){
  data %>% org_remapping %>% level_clean %>% resort_by_frequency(...)
}

#name doesn't mean much any more but it breaks apart
#a long string to something more displayable
break64 <- function(x){
  splitf <- function(string){
    string = substr(string, 1, 84)
    ss = strsplit(string, '/')[[1]]
    if (length(ss)==1 & nchar(string)>40){
      ss2 =strsplit(ss,' ')[[1]]
      i1 = ceiling(length(ss2)/2)
      return(paste(paste(ss2[1:i1], collapse=' '), 
                   paste(ss2[(i1+1):length(ss2)], collapse=' '),
                   sep='\n'))
    }
    if (length(ss)==1 | nchar(string) < 36)
      return(paste(ss, collapse='/'))
    half_index = ceiling(length(ss)/2)
    s1 = paste(ss[1:half_index], collapse='/')
    s2 = paste(ss[(half_index+1):length(ss)], collapse='/')
    return(paste(s1, s2, sep='/\n'))
  }
  return(sapply(x, splitf))
  
}

#may require some manual work for cases that actually
#do need a . in the name
level_clean <- function(data){
  x = data$variable
  from = levels(x)
  #cases that include a .
  to = gsub('Women.s','Women\'s', from)
  to = gsub('e\\.g\\.','FOREXAMPLE', to)
  to = gsub('sibling.s','siblings', to)
  to = gsub('I.ve',"I've", to)
  to = gsub('Co.workers','Coworkers',to)
  to = gsub('\\.+org\\.*anization',' organization', to)
  to = gsub('Non.profit','Non-profit', to)
  to = gsub('\\.org','DOTORG', to)
  to = gsub('Citizen.s','Citizen\'s', to)
  to = gsub('U\\.S\\.','UNITEDSTATES',to)
  #general cases
  to = gsub('\\.+$','', to)
  to = gsub('\\.{2}','/', to)
  to = gsub('\\.',' ', to)
  #revert cases that include a .
  to = gsub('DOTORG','.org.', to)
  to = gsub('FOREXAMPLE','e.g.', to)
  to = gsub('UNITEDSTATES','U.S.',to)
  #break lines apart
  to = break64(to)
  #some grammatical/spelling fixes
  to = gsub('/e\\.g\\./',' e.g., ', to)
  to = gsub('an\\.org\\.anization','an organization',to)
  to = gsub('the\\.org\\.anization','the organization',to)
  to = gsub('([Aa])nother\\.org\\.anization','\\1nother organization',to)
  to = gsub('/(\\s?)or','\\1or',to)
  to = gsub('e mails','emails',to)
  to = gsub('witness slips','Witness slips',to)
  x = mapvalues(x, from=from, to=to)
  data$variable = x
  return(data)
}

org_remapping <- function(data, rankFactors=TRUE){
  from = c('act.Blue','PROGRESSIVE.DEMOCRATS.OF.AMERICA','Indivisible.12th','Confluence.Progressives',
           'Action.Metro.East','Flippable','Swing.Left','Sister.District','Pantsuit.Nation',
           'Action.for.a.Better.Tomorrow','MoveOn.org','Women.s.March','Organizing.for.Action..OFA.',
           'Indivisible', 'Southwestern.IL.Dem.Women')
  to = c('ActBlue', 'Progressive Democrats of America','Indivisible 12th','Confluence Progressives',
         'Action Metro East','Flippable','Swing Left','Sister District','Pantsuit Nation',
         'Action for a Better Tomorrow','MoveOn.org',"Women's March",'Organizing for Action',
         'Indivisible','Southwestern Illinois Democrat Women')
  x = data$variable
  x = mapvalues(x, from=from, to=to)
  
  if (rankFactors){
    x = factorRank(x)
  }
  data$variable = x
  return(data)
}

factorRank <- function(x){
  tbl = table(x)
  tbl_names = names(tbl)
  return(factor(x, levels = tbl_names[order(tbl)]))
}

#some of the surveys have inconsistent 
identify_columns <- function(data, common_string, other_pair_rel=NULL){
  cnames = colnames(data)
  relevant_columns = which(grepl(common_string, cnames))
  other_pair = NULL
  if (!is.null(other_pair_rel)){
    lastindex = max(relevant_columns)
    other_pair = lastindex + other_pair_rel
    relevant_columns = relevant_columns[!relevant_columns %in% other_pair]
  }
  return(list(colrange=relevant_columns, other_pair=other_pair))
}

identify_and_expand <- function(data, common_string, other_pair_rel=NULL, ...){
  id.kwargs = identify_columns(data, common_string, other_pair_rel)
  return(expand_column_group(data, colrange=id.kwargs[['colrange']], 
                             other_pair=id.kwargs[['other_pair']],
                             ...))
}

fabricate_report <- function(filename, directory = 'Raw Data DO NOT SHARE', 
                             output_directory = 'reports', make_pdf=TRUE){
  #the below fields have changed position..updating
  #Have you heard of organizations? #56-70
  df <<- read.csv(file=paste(directory, filename, sep='/'))
  organizations_heardof <<- identify_and_expand(df, 'Have.you.heard.of.any.of.the.following.digital.grassroots.organizations.')
  #expand_column_group(df, colrange=56:70)
  
  #plot_survey_data(organizations_heardof, mode='yn')+ggtitle('test title')
  
  #ggplot(organizations_heardof %>% filter(value=='YES'), aes(x=variable)) + 
  #  geom_bar() +
  #  coord_flip() 
  
  # Have you participated in activities with these digital organizations in the past year?
  #71-84
  organizations_1y <<- identify_and_expand(df, 'In.the.past.year.have.you.ever.participated.in.activities.with.any.of.the.following.digital.organizations.')
  #expand_column_group(df, colrange=71:84)
  
  
  
  #85-98 - How often do you participate in activities of these organizations in the past 3 months?
  organizations_3m <<- identify_and_expand(df, 'Have.you.heard.of.any.of.the.following.digital.grassroots.organizations.')
  #expand_column_group(df, colrange=85:98)
  
  #99-121- - What activities do you do in these organizations (select all that apply)
  #123 - does not participate
  #122, 124 -other                                       'In.general..what.types.of.activities.do.you.participate.in.with.these.organizations.Please.select.all.that.apply.'
  organization_participation <<- identify_and_expand(df, 'In.general..what.types.of.activities.do.you.participate.in.with.these.organizations.Please.select.all.that.apply.',
                                                   other_pair_rel=c(-2,0))
  #expand_column_group(df, colrange=c(99:121,123), other_pair = c(122, 124))
  
  #125-147 - What activities do you prefer to do
  #148, 149 (other for above)
  organization_preference <<- identify_and_expand(df, 'What.types.of.activities.do.you.prefer.to.do.Please.select.all.that.apply.',
                                                other_pair_rel = c(-1, 0))
  #expand_column_group(df, colrange=125:147, other_pair = 148:149)
  
  #150 - free text (ideas for better engaging volunteers)
  engagement_ideas = df[,c(1,which(names(df)=="What.ideas.do.you.have.for.better.engaging.volunteers."))]
  
  #151-160 - How did you initially find your current organization?
  #161, 162 (other for above category)
  organization_initialcontact <<- identify_and_expand(df, 'How.did.you.initially.find.the.organizations.you.are.involved.with.Please.select.all.that.apply.',
                                                    other_pair_rel = c(-1, 0))
  #expand_column_group(df, colrange=151:160, other_pair=161:162)
  
  #163-172 - Which of these organizations would you consider volunteering for?
  #173, 174 (other for above category)
  party_volunteerconsideration <<- identify_and_expand(df, 'Which.of.the.following.types.of.organizations.would.you.consider.volunteering.for.Please.select.all.that.apply.',
                                                     other_pair_rel = c(-1, 0))
  #expand_column_group(df, colrange=163:172, other_pair = 173:174)
  
  #175-183 - Who do you talk to abooute these digital grassroots activities?
  #184, 186 (other for above category)
  #185 - does not discuss
  grassroots_discussion <<- identify_and_expand(df, 'Who.do.you.talk.with.about.your.digital.grassroots.activities.Please.select.all.that.apply.',
                                              other_pair_rel = c(-2, 0))
  #expand_column_group(df, colrange=c(175:183, 185), other_pair=c(184,186))
  
  
  #187-196 - How important are these issues to you (ranked from 1-5, 1=most important)
  important_issues <<- identify_and_expand(df, 'Please.rank.the.top.five..5..issues.that.are.most.important.to.you.with..1..being.the.most.important.and..5..being.the.least.important.',
                                         summary_type='ranked')
  #expand_column_group(df, colrange=187:196, summary_type='ranked')
  
  #197 (other important issue)
  other_important_issues <<- df[,c(1,which(names(df)=='What.other.issues.are.important.to.you.'))]
  
  #198-216 - top 3 most trusted news sources (ranked from 1-3, 1=most trustworthy)
  trustworthy_news <<- identify_and_expand(df, 'What.are.your.top.three..3..most.trusted.news.sources.',
                                         summary_type='ranked')
  #expand_column_group(df, colrange=198:216, summary_type='ranked')
  
  #217-219 - how active are you in organizations at lvl of govt compared to 1 year ago
  #note that variable names are full column names because of the question phrasing (still grouped bc possible responses are same)
  activity_1y <<- expand_column_group(df, 
                                    colrange=which(names(df)=="Compared.to.one.year.ago.today..how.active.are.you.in.organizations.that.target.City.and.County.issues." ) 
                                    + 0:2, summary_type='') %>%
    mutate(variable=as.character(variable),
           variable=gsub('.*City.*','City and County',variable),
           variable=gsub('.*State','State',variable),
           variable=gsub('.*Federal','Federal',variable),
           variable=factor(variable, levels=c('City and County','State','Federal')))
  #expand_column_group(df, colrange=217:219, summary_type='') %>%
  #220-227 - How much do you trust these officials compared to a year ago?
  electedofficial_trust1y = identify_and_expand(df, '.Compared.to.one.year.ago.today..how.much.do.you.trust.your.elected.officials.to.act.in.your.best.interest',
                                                summary_type='') %>%  clean_results(mode='')
  #expand_column_group(df, colrange=220:227, summary_type='') %>%
  # clean_results(mode='')
  
  
  #228-231 - How much trust do you have in the government compared to 1y ago?
  govt_trust1y <<- identify_and_expand(df, 'Compared.to.one.year.ago.today..how.much.do.you.confidence.do.you.have.in.the.government.',
                                     summary_type='')
  #expand_column_group(df, colrange=228:231, summary_type='')
  
  #232 - Who did you support in the last election?
  #233 (other for above category)
  candidate_main_idx <<- which(names(df)=="Who.did.you.support.in.the.Presidential.primary.election.in.2016.")
  candidate_other_idx <<- which(names(df)=="Other...Please.specify.Who.did.you.support.in.the.Presidential.primary.election.in.2016.")
  
  support_lastelection <<- cbind(data.frame(Response.ID=df[,1], 
                                          variable=ifelse(is_not_empty(df[,c(candidate_main_idx)]), 
                                                          as.character(df[,c(candidate_main_idx)]), 
                                                          as.character(df[,c(candidate_other_idx)])),
                                          value='YES'
  )
  ) %>% mutate(variable = as.character(variable), 
               variable=ifelse(is.na(variable), 'No Response', variable),
               variable=factor(variable))
  
  #234 - Which political party do you identify with?
  #235 (other for above)
  
  party_main_idx = which(names(df)=="Which.of.the.following.political.parties.do.you.most.strongly.identify.with.")
  party_other_idx = which(names(df)=="Other...Please.specify.Which.of.the.following.political.parties.do.you.most.strongly.identify.with.")
  
  
  party_identity <<- cbind(data.frame(Response.ID=df[,1],
                                    variable=ifelse(is_not_empty(df[,c(party_main_idx)]), as.character(df[,c(party_main_idx)]), 
                                                    as.character(df[,c(party_other_idx)])),
                                    value='YES')
  ) %>% mutate(variable = as.character(variable), 
               variable=ifelse(is.na(variable), 'No Response', variable),
               variable=factor(variable))
  
  #236 - How confident are you that dem party is moving in the right direction?
  dem_confidence = df[,c(1,which(names(df)=="How.confident.are.you.that.the.Democratic.Party.is.moving.in.the.right.direction."))] %>% mutate(value='YES')
  names(dem_confidence)[2] = 'variable'
  dem_confidence <<- dem_confidence %>% mutate(variable = mapvalues(variable, from='', to='No Response'))
  
  #237 - How much more/less progressive does dem party need to be?
  dem_progressiveneeds = df[,c(1,which(names(df)=="How.much.more.or.less.progressive.does.the.Democratic.Party.need.to.be.in.its.policy.initiatives."))] 
  names(dem_progressiveneeds)[2] = 'variable'
  dem_progressiveneeds <<- dem_progressiveneeds %>% mutate(variable=mapvalues(variable, from='', to='No Response'),
                                                         value='YES')
  dem_progressiveneeds$value = "YES"
  dem_progressiveneeds <<- dem_progressiveneeds
  
  #238 - What is the most important thing dem party can do to win more elections (text, I think)
  dem_winneeds <<- df[,c(1,which(names(df)=="What.is.the.most.important.thing.the.Democratic.Party.can.do.to.win.more.elections."))] %>%
    mutate(value="YES")
  
  #239 - What characteristics do you value in your elected officials (text, I think)
  official_valuedcharacteristics <<- df[,c(1,which(names(df)=="What.characteristics.do.you.value.in.your.elected.officials."))]
  
  #240 - Which local, state, or national elected official do you like the most?
  favorite_official <<- df[,c(1,which(names(df)=="Which.local..state..or.national.elected.official.do.you.like.the.most."  ))]
  
  #241 - Which local, state, or national elected official do you like the least?
  hated_official <<- df[,c(1,which(names(df)=="Which.local..state..or.national.elected.official.do.you.like.the.least."  ))]
  
  #242 - Additional comments
  additional_comments <<- df[,c(1,which(names(df)=="What.additional.comments..thoughts..or.concerns.would.you.like.to.share.with.us."))]
  
  
  #at this point, use these functions in .Rnw file...
  
  ###OUTDATED COLUMN INDICES
  #####DEMOGRAPHICS
  #203 - ZIP CODE
  #204 - Race/Ethnicity
  #205 - (Other Race/Ethnicity)
  #206 - Birthday
  #207 - Gender Identity
  #208 (other for above)
  #209 - Highest level of education
  #210 - Employment status
  #211 (other for above)
  #212 - Household income before taxes (2016)
  #213 - Street address
  #214 - Suite of Street Address
  #215 - City Address
  #216 - First name
  #217 Last name
  #218 - would you like voter file to be updated?
  
  tex_filename <- function(x) gsub('.csv','.tex',x)
  tex_output_filename = tex_filename(filename)
  if (make_pdf){
    fig.path = paste0('tex/',gsub('(.*).tex','figure_\\1',tex_output_filename))
    print(fig.path)
    #opts_knit$set(base.dir=fig.path)
    dir.create(fig.path, showWarnings=FALSE)
    knit('report_1.Rnw', output=paste0('tex/', tex_output_filename))
    system(paste('pdflatex',
                 paste0('-output-directory=',output_directory),
                 paste0('tex/', tex_output_filename)
                 )
    )
  }
}

pdfknit <- function(x='report_1.Rnw'){
  knit(x)
  system(paste('pdflatex',gsub('\\.Rnw','.tex',x)))
}