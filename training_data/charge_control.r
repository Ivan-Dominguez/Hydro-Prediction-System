library(mailR)

charge_Control<-function(){
send.mail(from="gavydong@gmail.com",
          to="gavydong@outlook.com",
          subject="Start charging",
          body="charging request",
          smtp=list(host.name = "smtp.gmail.com",
                    port = 465,
                    user.name = "gavydong@gmail.com",
                    passwd = "dong1835",
                    ssl = T),
          authenticate=T)
}
discharge_Control<-function(){
send.mail(from="gavydong@gmail.com",
          to="gavydong@outlook.com",
          subject="Start discharging",
          body="discharging request",
          smtp=list(host.name = "smtp.gmail.com",
                    port = 465,
                    user.name = "gavydong@gmail.com",
                    passwd = "dong1835",
                    ssl = T),
          authenticate=T)
}