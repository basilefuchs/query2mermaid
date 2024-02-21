query2mermaid = function(q) { 
  #q = querry
  q = str_replace_all(q,"\\n","")
  q = str_replace_all(q,"\\s+=\\s+","=")
  table = str_extract(q,"FROM.+WHERE")
  column = str_extract(q,"SELECT.+FROM")

  dt = str_extract_all(table,"\\w+\\.\\w+\\s\\w+") %>% unlist()%>% as.data.table() %>% setnames(old=".", new="table")

  dt[, shema := str_extract(table,"\\w+\\.")]
  dt[, table_alias := str_extract(table,"\\s\\w+")]

  dt[, table := str_replace(table,"\\w+\\.","")]
  dt[, table := str_replace(table,"\\s\\w+","")]
  dt[, table := str_replace(table,"\\s+","")]

  dt[, shema := str_replace(shema,"\\.","")]
  dt[, shema := str_replace(shema,"\\s+","")]

  dt[, table_alias := str_replace(table_alias,"\\.","")]
  dt[, table_alias := str_replace(table_alias,"\\s+","")]

  dt_table = dt[!table_alias %in% c('AND','OR')] %>% unique()

  dt = str_extract_all(table,"\\w+\\.\\w+=\\w+\\.\\w+") %>% unlist() %>% as.data.table() %>% setnames(old=".", new="text")

  dt[, table_alias1 := str_extract(text,".+=")]
  dt[, table_alias2 := str_extract(text,"=.+")]

  dt[, text := str_replace_all(text, "\\w+\\.","")]
  dt[, table_alias1 := str_replace_all(table_alias1, "=","")]
  dt[, table_alias1 := str_replace_all(table_alias1, "\\..+","")]
  dt[, table_alias1 := str_replace_all(table_alias1, "\\s+","")]
  dt[, table_alias2 := str_replace_all(table_alias2, "=","")]
  dt[, table_alias2 := str_replace_all(table_alias2, "\\..+","")]
  dt[, table_alias2 := str_replace_all(table_alias2, "\\s+","")]

  dt_join = dt %>% unique()

  dt = str_extract_all(column,"\\w+\\.\\w+") %>% unlist() %>% as.data.table() %>% setnames(old=".", new="column")

  dt[, table_alias := str_extract(column,"\\w+\\.")]

  dt[, column := str_replace(column,"\\w+\\.","")]
  dt[, column := str_replace(column,"\\s+","")]
  dt[, table_alias := str_replace(table_alias,"\\.","")]
  dt[, table_alias := str_replace(table_alias,"\\s+","")]

  dt_column = dt %>% unique()

  table = dt_table[dt_column, on=.(table_alias),.(table,column)] %>% unique()

  table[, column := paste0(column,' unknown "description"\n')]

  table = table[, .(column = paste0(column, collapse = "")), by=.(table)]

  table[, table := paste0(table,"{\n")]
  table[, column := paste0(column,"}")]
  table[, text := paste0(table,column)]

  table_bloc = paste0(table$text, collapse = "\n")

  join = dt_table[dt_join, on=c("table_alias==table_alias1"),.("table1"=table,text,table_alias2)]
  join = dt_table[join, on=c("table_alias==table_alias2"),.(table1,"table2"=table,text)]

  join = join[, .(text = paste0(text, collapse = ", ")), by=.(table1, table2)]

  join[, text2 := paste0(table1,' ||--|| ',table2,' : "',text,'"')]

  join_bloc = paste0(join$text2, collapse = "\n")

  final = paste("erDiagram",table_bloc,join_bloc, sep = "\n")

  cat(final)
}
