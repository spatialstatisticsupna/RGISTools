import sys
import os

#sys.path.append('C:/Users/unai.perez/.snap/snap-python')
snapydir = sys.argv[1]
print("Snap Dir: "+snapydir)
imagedir = sys.argv[2]
print("Image Dir: "+imagedir)
outdirname = sys.argv[3]
print("Output Dir: "+outdirname)
orchdir = sys.argv[4]
print("Working Dir: "+orchdir)

#sys.path.append('/home/unai.perez/.snap/snap-python')
sys.path.append(snapydir)
import snappy
from snappy import ProductIO
from snappy import HashMap
from snappy import GPF
import datetime
from datetime import datetime
import gc



os.chdir(orchdir)

#filenames=filenames[1:2]

manifestpath = os.path.join(imagedir, 'manifest.safe')
if os.path.isfile(manifestpath):
    filename=os.path.basename(imagedir)
    print("Manifest found!   " + filename)
    sentinel_1 = ProductIO.readProduct(manifestpath)
    sat = filename[:3]
    typen = filename[7:11]
    daten = filename[17:25]
    daten = datetime.strptime(daten, '%Y%m%d')
    daten = daten.strftime('%Y%j')

    pols = ['VH', 'VV']
    # p=pols[0]
    for p in pols:
        ### CALIBRATION
        polarization = p
        # parameters = HashMap()
        # parameters.put('outputSigmaBand', True)
        # parameters.put('sourceBands', 'Intensity_' + polarization)
        # parameters.put('selectedPolarisations', polarization)
        # parameters.put('outputImageScaleInDb', False)
        #
        # calib = oPath + "calibrate_" + polarization
        # target_0 = GPF.createProduct("Calibration", parameters, sentinel_1)
        # #ProductIO.writeProduct(target_0, calib, 'GeoTIFF')
        #
        # ### TERRAIN CORRECTION
        # parameters = HashMap()
        # parameters.put('demResamplingMethod', 'NEAREST_NEIGHBOUR')
        # parameters.put('imgResamplingMethod', 'NEAREST_NEIGHBOUR')
        # parameters.put('demName', 'SRTM 3Sec')
        # parameters.put('pixelSpacingInMeter', 10.0)
        # parameters.put('sourceBands', 'Sigma0_' + polarization)

        ### TERRAIN CORRECTION
        parameters = HashMap()
        parameters.put('imgResamplingMethod', 'BILINEAR_INTERPOLATION')
        parameters.put('sourceBands', 'Amplitude_' + p)
        parameters.put('mapProjection', 'AUTO:42001')

        # terrain = oPath + "_corrected_" + polarization
        # target_2 = GPF.createProduct("Terrain-Correction", parameters, target_0)
        # ProductIO.writeProduct(target_2, terrain, 'GeoTIFF')
        terrain = outdirname + "/" + sat + "_" + typen + "_" + daten + "_projected_Amplitude_" + polarization
        target_2 = GPF.createProduct("Ellipsoid-Correction-GG", parameters, sentinel_1)
        ProductIO.writeProduct(target_2, terrain, 'BEAM-DIMAP')  # 'GeoTiff')
else:
    print("Manifest not found for " + filename)
print("finished")