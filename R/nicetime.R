nicetime = function(seconds){
    if(length(seconds) == 0){
        return("")
    }
    lapseconds = round(seconds)
    daysinyear = 365.2425    # = gregorian year, julian year = 365.25
    years = lapseconds/(60*60*24*daysinyear)
    days = (years - floor(years)) * daysinyear
    hours = (days - floor(days)) * 24
    minutes = (hours - floor(hours)) * 60
    seconds = (minutes - floor(minutes)) * 60
    years = floor(years)
    days = floor(days)
    hours = floor(hours)
    minutes = floor(minutes)
    seconds = floor(seconds)
    lapline=""
    if(years!=0){
        if(years==1){lapline=paste(lapline,years,"y, ",sep="")      # year
        }else{lapline=paste(lapline,years,"y, ",sep="")             # years
        }
    }
    if(days!=0 | years!=0){
        if(days==1){lapline=paste(lapline,days,"d, ",sep="")        # day
        }else{lapline=paste(lapline,days,"d, ",sep="")              # days
        }
    }
    if(hours!=0 | days!=0 | years!=0){
        if(hours==1){lapline=paste(lapline,hours,"h, ",sep="")      # hour
        }else{lapline=paste(lapline,hours,"h, ",sep="")             # hours
        }
    }
    if(minutes!=0 | hours!=0 | days!=0 | years!=0){
        if(minutes==1){lapline=paste(lapline,minutes,"m, ",sep="")  # minute
        }else{lapline=paste(lapline,minutes,"m, ",sep="")           # minutes
        }
    }
    if(seconds==1){lapline=paste(lapline,seconds,"s",sep="")        # second
    }else{lapline=paste(lapline,seconds,"s",sep="")                 # seconds
    }
    return(lapline)
}

