!cc   * $Rev: 4877 $
!cc   * $Author: rcapote $
!cc   * $Date: 2017-04-03 21:12:41 +0200 (Mo, 03 Apr 2017) $

      INTEGER NDNUC,NDEXCLUS,NDEX,NDLW,LEVCC,NDLV,NDBR,NDMSCS
!
      PARAMETER(                                                        &
     & NDNUC= 100                                                       &
     &,NDEXCLUS= 50                                                     &
     &,NDEX= 101                                                        &
     &,NDLW= 100                                                        &
     &,LEVCC=40                                                         &
     &,NDLV=40                                                          &
     &,NDBR=40                                                          &
     &,NDMSCS=4)
!
!    DO NOT CHANGE parameters below 
!    unless you REALLY know what you are doing.
!    You may render the code unusable.
!
!
      INTEGER MAX_PRN,NDEJC,NDZMAX,NDECSE,NDEPFN,NDANGecis,NDAnghmx
      INTEGER NDNUCD,NDECSED,NDANGD,NDEX_D,NDEJCD,NLGRID
      INTEGER NDVOM,NDWOM,NDVSO,NDRVOM,NDRWOM,NDRVSO
      INTEGER NDETL,NDEREC,NDERO,NDCC,NDROPM
      INTEGER NDGDRPM,NDGQRPM,NDGMRPM
      INTEGER NDKNTR,NMAsse,NDREGIONS,NDMODELS,NDDEFCC,NDCOLLEV
      INTEGER NFtrans,NFMOD,NFisbarpnt,NFPARAB,NFHUMP,NFISENMAX
!
      PARAMETER(MAX_PRN=30,                                             &
     & NDEJC=6                                                          &
     &,NDZMAX=110                                                       &
!    &,NDECSE=NDEX+30                                                   &
     &,NDECSE=NDEX*1.30                                                 &
     &,NDEPFN=251                                                       &
     &,NDANGecis=91                                                     &
     &,NDAnghmx=37                                                      & !Must be equal to NDAnghms in ddhms.cmb
     &,NDNUCD=NDNUC                                                     &
     &,NDECSED=NDECSE                                                   &
     &,NDANGD=NDANGecis                                                 &
     &,NDEX_D=NDEX                                                      &
     &,NDEJCD=NDEJC                                                     &
     &,NLGRID=301                                                       & !Number of points in the HFB GDR RIPL files 

     &,NDVOM=7                                                          &
     &,NDWOM=7                                                          &
     &,NDVSO=7                                                          &
     &,NDRVOM=3                                                         &
     &,NDRWOM=3                                                         &
     &,NDRVSO=3                                                         &
!    &,NDETL=NDEX+6                                                     &
     &,NDETL=NDECSE                                                     &
     &,NDEREC=NDECSE                                                    &
     &,NDERO=NDEX                                                       &
     &,NDCC=10                                                          &
     &,NDROPM=7                                                         &
     &,NDGDRPM=10,NDGQRPM=8,NDGMRPM=8                                   &
     &,NDKNTR=3,NMAsse=9066                                             &
     &,NDREGIONS=14,NDMODELS=6,NDDEFCC=6,NDCOLLEV=99                    &     
!    &,NDREGIONS=14,NDMODELS=6,NDDEFCC=6,NDCOLLEV=80                    &
     &,NFtrans=40,NFMOD=3,NFisbarpnt=300                                &
     &,NFPARAB=5,NFHUMP=3,NFISENMAX=400)