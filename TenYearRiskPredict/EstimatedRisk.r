
CoAge<-12.344
CoTC<-11.853
CoAgeTC<- -2.664
CoHDL<- -7.990
CoAgeHDL<- 1.769


CoSmoker<- 7.837
CoAgeSmoker<- -1.795
CoDiabetes<- 0.658
MeanCoValue<-61.18
BaselineSur<-0.9144







EstimatedRisk<- function( age=55, TC=200, SBP=140, TreatOrNot="N",
                        diabetes="N", smoker="N",HDL = 100){
if(TreatOrNot=="N"){
		CoSBP<-1.764
		rSBP_value<-round(CoSBP*log(SBP),digits=2)
		}
		else{
            CoSBP<- 1.797
		rSBP_value<- round(CoSBP*log(SBP), digits =2)		
            }
   rAge_value<-round(CoAge*log(age), digits=2)
   rTC_value<-round(CoTC*log(TC),digits=2)
   rAgeTC_value<-round(CoAgeTC*log(age)*log(TC), digits=2)
   rHDL_value<-round(CoHDL*log(HDL), digits=2)
   rAgeHDL_value<-round(CoAgeHDL*log(age)*log(HDL), digits=2)
   if(smoker == "Y"){
	rSmoker_value<-round(CoSmoker*1,digits=2)
	rAgeSmoker_value<- round(-1.795*log(age)*1, digits=2)
	}else{
		rSmoker_value<- 0
		rAgeSmoker_value<- 0
	}
    if(diabetes == "Y"){
	rDiabetes_value<- round(0.658*1, digits=2)
	 }else{
		rDiabetes_value<- 0
	}

	IndividualSum<-rAge_value + rSBP_value + rTC_value + rAgeTC_value + rHDL_value + rAgeHDL_value + rSmoker_value+rAgeSmoker_value + rDiabetes_value
        
	risk<- 1-round((BaselineSur^exp(IndividualSum-MeanCoValue)), digits=3)
	return(risk)
			
			
			
			
			}
#EstimatedRisk(age=75, TC=140, HDL=40, SBP=180, TreatOrNot="Y",diabetes="N", smoker = "Y")
