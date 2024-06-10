#################################################################################
#*******************************************************************************#
#*				Fonction le calcul de la pauvrete et						   *#
#*				Fonction de simulation de l'intervalle de confiance			   *#
#*																			   *#
#*				rédigé par :  WAFFO DUBOUA PRINCE DONAT						   *#
#*				Date : Avril 2024											   *#
#*******************************************************************************#
#################################################################################


#########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+													   +#
#+         fonction pour le calcul de la pauvrete      +#
#+													   +#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#########################################################

povAF<-function(X,z,w,k){
	#test si il y a des parametres manquants
	if(missing(X) || missing(z) || missing(w) || missing(k)) stop("ERREUR !!! Parametres manquants pour l'execution de la fonction 'povAF'\n")
	#test si 'X' est une matrice ou un dataframe
	if(is.data.frame(X) || is.array(X)){
		X=as.data.frame(X)
		n=nrow(X)
		d=ncol(X)
		#test si les parametres 'z' et 'w' sont des vecteurs
		if(is.vector(z) && is.vector(w)){
			#test si les deux vecteurs sont bien des chiffres
			if(!(is.numeric(z) && is.numeric(w))) stop("ERREUR !!! Les parametres 'z' et 'w' doivemt etre numeriques\n")
			#test si les deux vecteurs sont de meme taille que le nombre de colonnes de la matrice
			if(length(z)==d && length(w)==d){
				#test si la somme du parametre 'w' est egal au nombre de colonnes de X
				if(sum(w)!=d) stop("ERREUR !!! La somme des elements du vecteur 'w' est non conforme\n")
				#test si le dernier parametre est un nombre
				if(!(is.double(k))) stop("ERREUR !!! Le parametre 'k' doit etre un reel\n")
				if(k>=1 && k<=d){
				
					#fonction qui calcul les ponderation de privation sur une ligne
					priv<-function(coll,i,j){
						condition<-coll>=i
						coll[!condition]=j
						coll[condition]=0
						return(coll)
					}
					#appliquer la fonction 'priv' aux colonnes de la matrice et
					#aux elements de 'w' et 'z' correspondants
					
					g0<-mapply(X,z,w,FUN=priv)
					
					#intensite de privation par indiv=sum des ponderations des indiv
					C<-g0%*%rep(1,d)

					
					#identification des pauvres
					phi<-as.numeric(C>=k)
					
					#nombre de pauvres
					npov<-sum(phi)
					#incidence de la pauvrete
					H=npov/n
					
					#vecteur de privations censurees
					Cs<-C*phi            #les non pauvres sont remplaces par 0          
					
					#matrice des privations censurees
					g0[Cs==0,]=0 #remplacer toutes les valeurs des non pauvres par 0
					
					#proportion moyenne d’intensité de privation parmi les pauvres
					A=sum(Cs)/(npov*d)
					#mesure de pauvreté ajusté suivant la méthode AF
					Mo=H*A
					result<-c(Mo,H,A)
					names(result)=c("Mo","H","A")
					return(result)
				}else cat("ERREUR !!! Le dernier parametre doit etre un reel compris entre 1 et ",d)
			}else cat("ERREUR !!! Tailles des parametres 'z' et/ou 'w' non conforme \n")
		}else cat("ERREUR !!! Les parametres 'z' et 'w' doivent etre des vecteurs\n")
	}else cat("ERREUR !!! Autre element trouve a la place de matrice ou dataframe\n")
}



#############################################################
#|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#|														   |#
#|	 Fonction de simulation de l'intervalle de confiance   |#
#|														   |#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|#
#############################################################


simulerIC<-function(database,typeSondage="null",strate=0,FUN,methoSim,times=1000,er=0.05,...){
	#test si au moins un argument obligatoire est manquant
	if(!(missing(database) || missing(FUN) || missing(methoSim))){
	
		
		#test si la base à utiliser est un data.frame
		if(is.data.frame(database)){
			#verifier que le bootstrap est associé a un type de sondage
			if(missing(typeSondage) && (tolower(methoSim)=="bootstrap")){
				cat("Type de sondage non specifiée\n\n")
				cat("Veuillez choisir un type de sondage :\n")
				cat("0 - SAS\n")
				cat("1 - SAT\n")
				repeat{
					s<-scan(what=integer(),n=1,quiet=TRUE)
					if((s==0)||(s==1)){
						ifelse (s==0,typeSondage<-"SAS",typeSondage<-"SAT")	
						break
					}else cat("Entrer le chiffre 0 ou 1\n")
				}
				#fin lecture de 'typeSondage'
			}
			#test si les arguments de type caractères correspondent à leur type
			if(is.character(typeSondage) && is.character(methoSim)){
				#test si au moins un parametre par defaut à été renseigné par l'utilisateur
				if(!(missing(strate) && missing(times) && missing(er))){
					#test si les arguments de type nombre correspondent à leur type
					if(is.numeric(strate) && is.numeric(times) && is.numeric(er)){
						#test si les arguments sont des vecteurs
						if (length(strate)>1){
							strate=strate[1]
							cat("warning : strate est un vecteur. 1er elment pris en charge \n")
						}
						if (length(times)>1){
							times=times[1]
							cat("warning : times est un vecteur. 1er elment pris en charge \n")
						}
						if (length(er)>1){
							er=er[1]
							cat("warning : er est un vecteur. 1er elment pris en charge \n")
						}
						#test si les arguments sont negatifs
						if(strate<0){
							strate=abs(strate)
							cat("Warning : strate<0 mais transforme en positf \n")
						}
						if(times<0){
							times=abs(times)
							cat("Warning : times<0 mais transforme en positf \n")
						}
						if(er<0){
							er=abs(er)
							cat("Warning : er<0 mais transforme en positf \n")
						}
						#test si er est entre 0 et 1
						if(er>1){
							cat("ERREUR !!! 'er' doit etre un reel inferieur à 1 \n")
							cat("Veuillez saisir une valeur correcte de 'er' \n")
							repeat{
								cat("er = ")
								er<-scan(what=double(),n=1,quiet=TRUE)
								ifelse((er>=0 && er<=1), break, cat("Enter un reel entre 0 et 1\n"))
							}
							#fin lecture de 'er'
						}
					}else stop("ERREUR !!! 'strate' et/ou 'times' et/ou 'er' n'est pas un nombre. \n")
				}
				#fin test sur les valeurs par defaut
				
				#test si les arguments typeSondage et methoSim sont des vecteurs
				if (length(typeSondage)>1){
					typeSondage=typeSondage[1]
					cat("warning : typeSondage est un vecteur. 1er elment pris en charge \n")
				}
				if (length(methoSim)>1){
					methoSim=methoSim[1]
					cat("warning : methoSim est un vecteur. 1er elment pris en charge \n")
				}
				#fin test
				
				#uniformisation des caractères en format majuscule/minuscule
				typeSondage<-toupper(typeSondage)
				methoSim<-tolower(methoSim)
				#fin uniformisation
				
				
				if(methoSim %in% c('jackknife','bootstrap')){
					n=nrow(database)
					#considerer uniquement les variables numeriques
					base<-database[,sapply(database,FUN=is.numeric)]
					#si la methode de simulation est bootstrap
					if(methoSim=="bootstrap"){
						#test de coherence
						if(!(typeSondage %in% c("SAS","SAT"))) stop("ERREUR !!! Le type de sondage doit etre soit 'SAS' soit 'SAT' \n")
							
						#test si le type de sondage est aleatoire simple ou s'il n'y a aucune variable de stratification
						if((typeSondage=="SAS" && strate==0) || strate==0){
							
							###############################################
							#											  #
							#~~~~~~~~~~Sondage aleatoire simple~~~~~~~~~~~#
							#											  #
							###############################################
							
							#tester si l'utilisateur s'est trompé de type de sondage
							if(typeSondage=="SAT"){
								cat("Warning : sondage aleatoire simple effectue bien que specifie une methode SAT, car absence d'une variable de stratification \n")
								typeSondage="SAS"
							}
							
							#---------------------------
							#fonction qui renvoit la valeur du parametre calculé par FUN pour un echantillon i
							vectech<-function(i){
								vect=sample(1:n,n,T) #vecteur contenant les numeros des indinvidus tiré avec remise
									
								res=FUN(base[vect,],...)
								return(res[1])
							} #fin "vectech"
							#---------------------------
								
							#vecteur contenant les times valeurs calculées par FUN							
							result<-sapply(1:times,vectech)
							

							#-----fin calcul sur echantillonnage et resultat par la 'methode bootstrap/SAS'-----#
								
						}else { #le type de sondage est la stratification ou il y a une variable de stratification
							
							###################################
							#								  #
						    #~~~~~~~~stratification~~~~~~~~~~~#
							#								  #
							###################################
								
								
							#tester si l'utilisateur s'est trompé de type de sondage
							if(typeSondage=="SAS"){
								cat("Warning : sondage stratifie effectue bien que specifie une methode SAS, car presence d'une variable de stratification \n")
								typeSondage="SAT"
							}
							#groupes d'indices par variable de stratificaation
							group<-split(1:n, database[,strate])
							
							#---------------------------------------
							#fonction qui renvoit la valeur du paramtre calculée sur un echatillon SAT avec remise
							vectech2<-function(i){
								
								#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
								#fonction qui à partir d'un vecteur de numeros renvoit un echantillon SAS avec remise de ces numeros
								stratifiernum<-function(subvect){
									len<-length(subvect)
									if(len==1){
										num<-subvect
									}else num<-sample(subvect,len,T) #num contient les numeros echantillonnés
								} #fin "stratifiernum"
								#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
									
								#'lapply' applique 'stratifiernum' sur la liste des '1:k' groupés
								# par la variable de stratification de 'database' et renvoit une liste
								vect=unlist(lapply(group, stratifiernum))
									
								res=FUN(base[vect,],...)
								return(res[1])
							} #fin "vectech2"
							#----------------------------------------
								
							#vecteur contenant les times valeurs calculées par FUN
							result<-sapply(1:times,vectech2)
							

							#-----fin calcul et resultat sur echantillonnage par la 'methode bootstrap/SAT'-----#

						}	
					}else {	   
						##################################################
						#											     #
					    #~~~~~~~~~~~~~~methode jackknife~~~~~~~~~~~~~~~~~#
						#												 #
						##################################################
						
						if(typeSondage=="SAS")  cat("Le 'SAS' n'est pas necessaire pour la methode 'jackknife'\n")
						if(typeSondage=="SAT")  cat("Le 'SAT' n'est pas necessaire pour la methode 'jackknife'\n")
						
						#fonction qui supprime la i-ième observation d'un dataframe
						extract<-function(i){
							db=base[-i,]
							res=FUN(db,...)
							return(res[1])
						}
						#vecteur contenant les n valeurs calculées par FUN
						result<-sapply(1:n,extract) 
							
						#-----fin calcul et resultat sur echantillonnage par la 'methode jackknife'-----# 
					}
					#Quantiles calculés sur le vecteur 'result'
					q1=quantile(result,er/2)
					q2=quantile(result,1-(er/2))
						
					#niveau de confiance IC
					n_IC=paste((1-er)*100,"%",sep="")
						
					#la valeur du paramètre cible calculée sur les données
					val_parametre=FUN(base,...)
					val<-val_parametre[1]
						
					#liste des resultats finaux
					resultat<-list(donnees=database,methode_de_simulation=methoSim,Nom_fonction="povAF",
									valeur_parametre=val,Intervalle_de_Confiance=c(q1,q2),
									niveau_de_confiance_IC=n_IC,
									valeurs_parametre_reechantillonnage=result)
									
					#la fonction simulerIC renvoit la liste 'resultat'
					return(resultat)
					
					#~~~~~~~~~~~~~~FIN~~~~~~~~~~~~~~#
					
				}else cat("ERREUR !!! La methode de simulation doit etre soit 'jackknife' soit 'bootstrap' \n")
			}else cat("ERREUR !!! 'typeSondage' ou 'methoSim' n'est pas une chaine de caractères \n")
		}else cat("ERREUR !!! 'database' doit etre de type 'data.frame' \n")
	}else cat("ERREUR !!! Il manque certains parametres pour l'execution de la fonction 'simulerIC' \n")
}