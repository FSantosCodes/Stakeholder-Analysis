library(StakeholderAnalysis)

#grupos de involucrados
grupos <- c("MAGAP","Comunidad","CP-Cotopaxi","JP-Guaytacama","ONG",
            "Intermediarios","Iglesia","Floricolas","Junta agua-riego","Banco comunal",
            "Proveedores","Mercado")
#suma y an uncion del total de involucrados
print(paste0("tiene un total de ",length(grupos)," involucrados"))

#respuestas sobre la ACTITUD de los involucrados frente al proyecto
actitud <- data.frame(act1=c(4,3,4,1,4,2,1,3,4,3,3,1),
                      act2=c(5,2,3,2,3,1,2,4,3,2,2,2),
                      act3=c(5,2,4,1,4,2,2,3,4,3,3,2),
                      act4=c(4,3,3,2,3,1,1,4,3,2,2,1))
#respuestas sobre la PODER de los involucrados frente al proyecto
poder <- data.frame(pod1=c(5,4,4,3,3,3,3,3,3,3,4,2),
                    pod2=c(4,5,3,4,2,4,4,2,4,3,3,3),
                    pod3=c(5,5,4,3,3,3,3,3,3,2,2,4),
                    pod4=c(5,4,3,4,2,4,4,2,4,2,3,3))
#respuestas sobre la URGENCIA de los involucrados frente al proyecto
urgencia <- data.frame(urg1=c(5,4,3,4,4,1,2,1,4,3,5,4),
                       urg2=c(4,5,4,3,5,2,3,2,3,4,4,3),
                       urg3=c(4,5,3,4,4,1,2,1,4,3,5,4),
                       urg4=c(5,4,4,3,5,2,3,2,3,4,4,3))
#respuestas sobre la LEGITIMIDAD de los involucrados frente al proyecto
legitimidad <- data.frame(leg1=c(5,4,5,4,3,2,1,2,3,1,3,2),
                          leg2=c(4,5,4,5,2,1,2,1,4,2,4,1),
                          leg3=c(5,4,4,5,3,1,2,1,3,2,4,2),
                          leg4=c(4,5,5,4,2,2,1,2,4,1,3,1))

#respuestas sobre el COSTO para los involucrados frente al proyecto
costo <- data.frame(cos1=c(3,2,2,3,3,4,3,5,3,2,2,2),
                    cos2=c(2,1,1,2,2,5,2,4,2,2,1,1),
                    cos3=c(3,2,2,2,3,4,3,4,3,3,2,2),
                    cos4=c(2,1,1,3,2,5,2,4,2,3,1,2),
                    cos5=c(3,2,2,2,3,4,3,5,2,2,1,1))
#respuestas sobre el BENEFICIO de los involucrados frente al proyecto
beneficio <- data.frame(ben1=c(5,4,4,3,4,2,3,2,4,4,3,4),
                        ben2=c(4,5,3,4,3,1,2,1,3,4,4,3),
                        ben3=c(5,5,4,3,3,1,3,2,3,3,3,4),
                        ben4=c(4,4,3,4,3,1,2,1,3,3,4,3),
                        ben5=c(4,5,3,3,4,2,3,2,4,3,3,4))

#respuestas sobre el my de los involucrados frente al proyecto
my <- data.frame(my1=c(5,4,4,3,4,2,3,2,4,4,3,4),
                 my2=c(4,5,3,4,3,1,2,1,3,4,4,3),
                 my3=c(5,5,4,3,3,1,3,2,3,3,3,4),
                 my4=c(4,4,3,4,3,1,2,1,3,3,4,3),
                 my5=c(4,5,3,3,4,2,3,2,4,3,3,4),
                 my6=c(4,5,3,3,4,2,3,2,4,3,3,4))

me <- data.frame(me1=c(3,2,2,3,3,4,3,5,3,2,2,2),
                 me2=c(2,1,1,2,2,5,2,4,2,2,1,1),
                 me3=c(3,2,2,2,3,4,3,4,3,3,2,2),
                 me4=c(2,1,1,3,2,5,2,4,2,3,1,2),
                 me5=c(3,2,2,2,3,4,3,5,2,2,1,1),
                 me6=c(3,2,2,2,3,4,3,5,2,2,1,1))

DataExp <- cbind(grupos,actitud,poder,urgencia,legitimidad,costo,beneficio,me,my)


# then execute PrelCalc(), RespVerif(), AttibIdent(), CollabPotential()
PrelCalcExp <- PrelCalc(data=DataExp, NoAtt=2:5,NoPow=6:9,
                        NoUrg=10:13,NoLeg=14:17,NoBen=18:22,NoCos=23:27)

RespVerifExp <- RespVerif(CountResponses=PrelCalcExp$CountResponses,
                       NoStakeholders=PrelCalcExp$NoStakeholders)


AttribIdentExp <- AttribIdent(TestedResponses=RespVerifExp,
                           NoAttrib=PrelCalcExp$NoAttrib, NoStakeholders=PrelCalcExp$NoStakeholders,
                           NameStakeholders=PrelCalcExp$NameStakeholders)

CollabPotentialExp <- CollabPotential(AttribIdent=AttribIdentExp)
CollabPotentialExp$Mean


BenefCostExp <- BenefCost(CountResponses=PrelCalcExp$CountResponses)
BenefCostExp

StakeholdClassifByMean <- StakeholdClassif(BenefCostExp$BenefCostTest,
                                           CollabPotentialExp$Mean,
                                           AttribIdentExp$Mean)

#plots
Histograms(path="",tofile=0,CountResponses=PrelCalcExp$CountResponses)
AttribPict(path="",tofile=0,AttribIdent=AttribIdentExp$Mean,CollabPotential=CollabPotentialExp$Mean)


