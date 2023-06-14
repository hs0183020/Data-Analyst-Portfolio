from oct_converter.readers import E2E
import time
start_time = time.time()

filepath = r'\\uthouston.edu\uthsc\SPH\Research\CollaboratorsCRU\Retina Heidelberg Spectralis OCT images\Complete Backup until 030723\10Y0401B.E2E'
file = E2E(filepath)

file.read_fundus_image()

patiendid=file.patient_id

print("--- %s seconds ---" % (time.time() - start_time))