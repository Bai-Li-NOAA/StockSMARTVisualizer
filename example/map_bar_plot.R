for(i in 1:nrow(science_centers)){

  if (science_centers$Science.Center[i] %in% c("PIFSC", "SWFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]-line_distance,
             y1=science_centers$Latitude[i],
             col="red", lty=2)

    text((science_centers$Longitude[i]-line_distance)*1.01,
         science_centers$Latitude[i]*1.07,
         science_centers$Science.Center[i], col="red")

    if ((science_centers$Science.Center[i] %in% rownames(barplot_data))) {
      plot_floating_bars(x=barplot_data[science_centers$Science.Center[i],],
                         xllc=(science_centers$Longitude[i]-line_distance)*1.03,
                         yllc=science_centers$Latitude[i]*0.8,
                         barwidth=barwidth, maxheight=maxheight, col=col)

      if ("SWFSC / PIFSC" %in% rownames(barplot_data)) {
        plot_floating_bars(x=barplot_data["SWFSC / PIFSC",],
                           xllc=(science_centers$Longitude[science_centers$Science.Center=="SWFSC"]-line_distance)*1.1,
                           yllc=(science_centers$Latitude[science_centers$Science.Center=="SWFSC"]-line_distance/3)*0.6,
                           barwidth=barwidth, maxheight=maxheight, col=col)
        text(x=(science_centers$Longitude[science_centers$Science.Center=="SWFSC"]-line_distance)*0.9,
             y=(science_centers$Latitude[science_centers$Science.Center=="SWFSC"]-line_distance/3)*0.5,
             "SWFSC/PIFSC")
      }
    }
  }

  if (science_centers$Science.Center[i] %in% c("NEFSC", "SEFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]+line_distance,
             y1=science_centers$Latitude[i],
             col="red", lty=2)
    text((science_centers$Longitude[i]+line_distance)*1.05,
         science_centers$Latitude[i]*1.07,
         science_centers$Science.Center[i], col="red")

    if ((science_centers$Science.Center[i] %in% rownames(barplot_data))){
      plot_floating_bars(x=barplot_data[science_centers$Science.Center[i],],
                         xllc=(science_centers$Longitude[i]+line_distance)*1.07,
                         yllc=science_centers$Latitude[i]*0.8,
                         barwidth=barwidth, maxheight=maxheight, col=col)
    }
  }

  if (science_centers$Science.Center[i] %in% c("NWFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]-line_distance,
             y1=science_centers$Latitude[i]-line_distance/3,
             col="red", lty=2)
    text((science_centers$Longitude[i]-line_distance)*1.05,
         (science_centers$Latitude[i]-line_distance/3)*1.03,
         science_centers$Science.Center[i], col="red")

    if ("NWFSC / AFSC" %in% rownames(barplot_data)) {
      plot_floating_bars(x=barplot_data["NWFSC / AFSC",],
                         xllc=(science_centers$Longitude[science_centers$Science.Center=="NWFSC"]-line_distance)*1.3,
                         yllc=(science_centers$Latitude[science_centers$Science.Center=="NWFSC"]+line_distance/3)*0.8,
                         barwidth=barwidth, maxheight=maxheight, col=col)

      text(x=(science_centers$Longitude[science_centers$Science.Center=="NWFSC"]-line_distance)*1.2,
           y=(science_centers$Latitude[science_centers$Science.Center=="NWFSC"]+line_distance/3)*0.75,
           "NWFSC / AFSC")
    }

    if ("NWFSC / SWFSC" %in% rownames(barplot_data)) {
      plot_floating_bars(x=barplot_data["NWFSC / SWFSC",],
                         xllc=(science_centers$Longitude[science_centers$Science.Center=="NWFSC"]-line_distance)*1.2,
                         yllc=(science_centers$Latitude[science_centers$Science.Center=="NWFSC"]-line_distance/3)*0.75,
                         barwidth=barwidth, maxheight=maxheight, col=col)

      text(x=(science_centers$Longitude[science_centers$Science.Center=="NWFSC"]-line_distance)*1.15,
           y=(science_centers$Latitude[science_centers$Science.Center=="NWFSC"]-line_distance/3)*0.7,
           "NWFSC / SWFSC")
    }

  }

  if (science_centers$Science.Center[i] %in% c("AFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]-line_distance,
             y1=science_centers$Latitude[i]+line_distance/3,
             col="red", lty=2)
    text((science_centers$Longitude[i]-line_distance)*1.05,
         (science_centers$Latitude[i]+line_distance/3),
         science_centers$Science.Center[i], col="red")

    if ((science_centers$Science.Center[i] %in% rownames(barplot_data))) {
      plot_floating_bars(x=barplot_data[science_centers$Science.Center[i],],
                         xllc=(science_centers$Longitude[i]-line_distance)*1.2,
                         yllc=(science_centers$Latitude[i]+line_distance/3)*0.9,
                         barwidth=barwidth, maxheight=maxheight, col=col)
    }
  }


}

legend("bottomleft",
       title=legend_title,
       pch=22, col="black",
       pt.bg=col,
       legend=key_model, bty="n")
degAxis(1)
degAxis(2, las=2)
box()
