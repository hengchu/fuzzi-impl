from setuptools import setup, find_packages

__VERSION__ = '0.0.1'

setup(name='fuzzi_gen',
      version=__VERSION__,
      packages=find_packages(),
      include_package_data=True,
      entry_points={
          'console_scripts': [
              'fuzzi-preprocess-mnist=fuzzi.processing.mnist:main',
              'fuzzi-preprocess-pate=fuzzi.processing.pate:main',
              'fuzzi-preprocess-spam=fuzzi.processing.spam:main',
              'fuzzi-preprocess-iris=fuzzi.processing.iris:main',

              'fuzzi-eval-mnist=fuzzi.evaluation.mnist:main',
              'fuzzi-eval-spam=fuzzi.evaluation.spam:main',
              'fuzzi-eval-iris=fuzzi.evaluation.iris:main',
              'fuzzi-eval-pate=fuzzi.evaluation.pate:main',
          ]
      }
)
