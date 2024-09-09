#!/usr/bin/env python3
import subprocess
import sys
"""
Create gnuplot script to produce pdf file with correlation matrices,
prior and posterior cross sections, and uncertainties.
This script is invoked by GUI when file with extension '.d' is
doubleclicked on GUI-tab 'Files'.
"""

def createCovariancePlot():
   #
   # Create the Gnuplot script content for covariance
   #
   title = result[0] + " " + MT + "x" + MT1
   if str(MT) in (["1", "2", "102"]): 
      setLogScaleX = 'set logscale x'
   else : 
      setLogScaleX = 'unset logscale x'
   if str(MT1) in (["1", "2", "102"]): 
      setLogScaleY = 'set logscale y'
   else :
      setLogScaleY = 'unset logscale y'
      
   pdfPlot = result[0] + "-" + MT + "x" + MT1 + "-corrplot.pdf"
   gnuplot_script = f"""
   set term postscript enhanced color font "Helvetica, 26"
   set output "corrplot.ps
   set size square
   set ticslevel 1
   set xlabel 'MT={MT1} Energy [MeV]'
   set ylabel 'MT={MT} Energy [MeV]'
   set zlabel 'Correlation [%]'
   set format z ''
   {setLogScaleX}
   {setLogScaleY}
   set title '{title}'
   set pm3d map
   set cbrange [-100:100]
   splot[0.01:20][0.01:20] '{file}' u 1:2:3 not w pm3d
   reset
   """
   # Write the script to a temporary file
   with open("corrplot.gnu", "w") as f:
      f.write(gnuplot_script)
   # Run Gnuplot on the script
   subprocess.run(["gnuplot", "corrplot.gnu"])
   subprocess.run(["ps2pdf", "corrplot.ps", pdfPlot])
   subprocess.run(["rm", "corrplot.ps", "corrplot.gnu"])
   
   gnuplot_script = ""
   return MT, MT1


def createCrossSectionPlot(MT):
   #
   # Create the Gnuplot script content for cross sections
   #
   title = result[0] + " MT=" + MT
   expxscplot = result[0] + "-" + MT + "-c4.gpd"
   xscplot = result[0] + "-" + MT + "-xspl.d"
   pdfPlot = result[0] + "-" + MT + "-xspl.pdf"
   if str(MT) in (["1", "2", "102"]): 
      setLogScaleX = 'set logscale x'
      setLogScaleY = 'set logscale y'
   else : 
      setLogScaleX = 'unset logscale x'
      setLogScaleY = 'unset logscale y'
   
   gnuplot_script = f"""
   set term postscript enhanced color font "Helvetica, 26"
   set output "corrplot.ps
   set xlabel 'Energy [MeV]' 
   set ylabel 'Cross Section [barn]'
   #set xrange [ 0 : * ]
   #set yrange [ 0 : * ]
   {setLogScaleX}
   {setLogScaleY}
   set title '{title}'
   set style line 1 lw 2 lt rgb "blue" 
   set style line 2 lw 2 lt rgb "red" 
   plot[][] '{expxscplot}' u 1:2:3 not w yerr, '{xscplot}' u 1:2 t 'posterior' w l ls 2 ,'' u 3:4 t 'prior' w l ls 1 
   """
   # Write the script to a temporary file
   with open("corrplot.gnu", "w") as f:
      f.write(gnuplot_script)
   # Run Gnuplot on the script
   subprocess.run(["gnuplot", "corrplot.gnu"])
   subprocess.run(["ps2pdf", "corrplot.ps", pdfPlot])
   subprocess.run(["rm", "corrplot.ps", "corrplot.gnu"])

   gnuplot_script = ""


def createErrorPlot(MT):
   #
   # Create the Gnuplot script content for uncertainty plot 
   #
   title = result[0] + " MT=" + MT
   xscplot = result[0] + "-" + MT + "-err.d"
   expxscplot = result[0] + "-" + MT + "-c4.gpd"
   pdfPlot = result[0] + "-" + MT + "-err.pdf"
   gnuplot_script = f"""
   set term postscript enhanced color font "Helvetica, 26"
   set output "corrplot.ps
   set title '{title}'
   set yrange [0:*]
   setLogScale = 'unset logscale'
   set ylabel 'Uncertainties [%]'
   set xlabel 'Energy [MeV]' 
   set style line 2 lw 2 lt rgb "red" 
   plot[][] '{expxscplot}' u 1:(100*($3/$2)) t 'experimental' w p pt 3 ps 1, '{xscplot}' u 1:2 t 'evaluated' w l ls 2 
   """
   with open("corrplot.gnu", "w") as f:
      f.write(gnuplot_script)
   # Run Gnuplot on the script
   subprocess.run(["gnuplot", "corrplot.gnu"])
   subprocess.run(["ps2pdf", "corrplot.ps", pdfPlot])
   subprocess.run(["rm", "corrplot.ps", "corrplot.gnu"])

# Access the script name (first element) and remaining arguments
script_name, *args = sys.argv

if __name__ == "__main__":

   def process_parameters(args):
      pass

   if  len(args) == 0 :
      file = input("Enter the file path: ")  # Process the arguments here
   else:
      file = args[0]

   # Decifer file path meaning
   result = file.split("-")
   print (result)
   title = result[0]
   qualifier = result[-1]

   if qualifier == "corrplot.d" :
      MT, MT1 = result[1].split("x")
      createCovariancePlot()
      if MT == MT1 :
         createCrossSectionPlot(MT)
         createErrorPlot(MT)
   elif qualifier == "xspl.d" :
         MT = result[1]
         createCrossSectionPlot(MT)
   elif qualifier == "err.d" :
         MT = result[1]
         createErrorPlot(MT)

   exit()


