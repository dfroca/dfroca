#install.packages("CLVTools")
#install.packages("devtools")

# Load the packages
library(RODBC)
library(CLVTools)

# PARAMETERS TO DEFINE IF THE PLOTS OF THE MODELS WILL BE EXPORTED
export_plots = FALSE
route = "/Users/danielroca/Desktop"


# Open the connection and select the database
db = odbcConnect("myodbca",uid="root",pwd="")
sqlQuery(db, "USE ma_charity_full")

# Define the cohort for which the R-script will be executed
# cohort = 1

# Closing date
cl_date = 20161231

for (cohort in seq(1,16)){
  # Data Frame the coefficient data is going to be stored
  df = data.frame(cohort = integer(), solicits = integer(), r = double(),
                  alpha = double(), s = double(), beta = double(),
                  spending = double())
  
  # Iterate through the number of contacted times 
  # c(0,1,2,3,4,5,6,7,8,9)
  for (num_contacted in seq(0,9)){
    
    if (num_contacted==0){
      query = "
    select a.contact_id, a.act_date, a.amount
    from acts a
    inner join (
    select a.contact_id
    , timestampdiff(year, min(a.act_date), %s) as cohort
    from acts a
    left join (
    select contact_id
    from actions
    where campaign_id in ('C164','C153','C136','C137','C138','C139','C141','C155','C142'
					               ,'C144','C145','C146','C175','C148','C170')
		group by contact_id) b on a.contact_id = b.contact_id
		where a.act_type_id = 'DO' and a.act_date <= %s
		and b.contact_id is null
		group by a.contact_id
		having cohort = %s
		) t on t.contact_id = a.contact_id
		where a.act_type_id = 'DO'
		order by a.contact_id asc, a.act_date asc
    "
      query = sprintf(query, as.character(cl_date), as.character(cl_date), as.character(cohort))
      
    }
    else{
      query = "select a.contact_id, a.act_date, a.amount
    from acts a
    inner join (
    select c.contact_id
    ,c.cohort
    ,count(1) q 
    from (
    select contact_id
    , timestampdiff(year, min(act_date), %s) as cohort
    from acts
    where act_type_id = 'DO' and act_date <= %s
    group by contact_id
    having cohort = %s
    ) c
    inner join actions a2 on a2.contact_id = c.contact_id
    where a2.campaign_id in ('C164','C153','C136','C137','C138','C139','C141','C155','C142'
						                ,'C144','C145','C146','C175','C148','C170')
		group by c.contact_id, c.cohort
		having q = %s
		) t on t.contact_id = a.contact_id
		where a.act_type_id = 'DO'
		order by a.contact_id asc, a.act_date asc
    "
      
      query = sprintf(query, as.character(cl_date), as.character(cl_date), as.character(cohort), as.character(num_contacted))
      
    }
    
    # Execute query
    data = sqlQuery(db, query)
    
    # Prepare data in the right format -> CLV Data format to use the Pareto NBD Model
    charity.data = clvdata(data.transactions = data, date.format = "ymd", time.unit = "year", name.id = "contact_id", name.date = "act_date", name.price = "amount")
    
    # Estimate model Pareto-NBD Model
    est.pnbd = pnbd(clv.data = charity.data, optimx.args = list(method="Nelder-Mead"))
    
    # Estimate Gamma-Gamma Model
    est.gg = gg(clv.data = charity.data)
    
    df1 =  data.frame(cohort, num_contacted, coef(est.pnbd)[1], coef(est.pnbd)[2],
                      coef(est.pnbd)[3],coef(est.pnbd)[4],mean(est.gg@cbs[["Spending"]]))
    
    names(df1) =  c("cohort","solicits", "r", "alpha", "s", "beta", "spending")
    df = rbind(df, df1)
    
    if (export_plots) {
      
      # Fits data weekly to plot
      plot.data = clvdata(data.transactions = data, date.format = "ymd", time.unit = "week", name.id = "contact_id", name.date = "act_date", name.price = "amount")
      est.pnbd2 = pnbd(clv.data = plot.data, optimx.args = list(method="Nelder-Mead"))
      
      name1 = paste( cl_date, cohort, as.character(num_contacted), "PNBD", sep = "-")
      name1 = paste( route, name1, sep = "/")
      name1 = paste( name1, "jpeg", sep = ".")
      
      jpeg(filename=name1)
      plot(est.pnbd2)
      dev.off()
    }
    
    print(paste("Cohort ·",as.character(cohort), " Iteration ·", as.character(num_contacted + 1), " finished!", sep=""))
    
  }
  
  # Final fixes to the dataframe after looping through all the solicitation numbers
  rownames(df) =  NULL
  
  # Computation of pareto nbd to interpret cohort results
  df$trans =  with(df, r/alpha)
  df$attri =  with(df, s/beta)
  df = transform(df, attri= ifelse(attri>1, 0.999, attri))
  
  # Export files
  filename = paste("results_C",cohort,sep="-")
  filename = paste(filename, "csv",sep=".")
  csv =  paste(route, filename,sep="/")
  
  write.csv(df, csv, row.names = FALSE)
  
}

plot(est.pnbd2)

