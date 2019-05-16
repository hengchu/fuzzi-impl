INCLUDE=fuzzi-lib/stdexts.fuzzi

.PHONY: fuzzi
fuzzi:
	stack haddock -j6

.PHONY: preprocess
preprocess:
	fuzzi-preprocess-mnist
	fuzzi-preprocess-iris
	fuzzi-preprocess-pate
	fuzzi-preprocess-spam

.PHONY: transpile
transpile:
	@echo ">>> transpiling mnist code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/mnist784.fuzzi \
			    -t fuzzi-gen/fuzzi/data/MNIST/mnist784.json \
			    > fuzzi-gen/fuzzi/generated/mnist784.py
	@echo ">>> transpiling naive bayes code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/nb.fuzzi \
			    -t fuzzi-gen/fuzzi/data/spambase/spam.json \
			    > fuzzi-gen/fuzzi/generated/nb.py
	@echo ">>> transpiling iris code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/kmeans.fuzzi \
			    -t fuzzi-gen/fuzzi/data/Iris/iris.json \
			    > fuzzi-gen/fuzzi/generated/iris.py
	@echo ">>> transpiling PATE code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/pate784.fuzzi \
			    -t fuzzi-gen/fuzzi/data/pate/pate.json \
			    > fuzzi-gen/fuzzi/generated/pate784.py
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/pate_label.fuzzi \
			    -t fuzzi-gen/fuzzi/data/pate/pate_test.json \
		 	    > fuzzi-gen/fuzzi/generated/pate_label.py

.PHONY: typecheck
typecheck:
	@echo ">>> typechecking mnist code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/mnist784.fuzzi \
			    | python3.7 -m json.tool
	@echo ">>> typechecking naive bayes code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/nb.fuzzi \
			    | python3.7 -m json.tool
	@echo ">>> typechecking iris code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/kmeans.fuzzi \
			    | python3.7 -m json.tool
	@echo ">>> typechecking PATE code..."
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/pate784.fuzzi \
			    | python3.7 -m json.tool
	stack exec -- fuzzi -I $(INCLUDE) \
			    -f examples/pate_label.fuzzi \
			    | python3.7 -m json.tool

.PHONY: evaluate
evaluate:
	@echo ">>> running logistic regression evaluation..."
	fuzzi-eval-mnist
	@echo ">>> running naive bayes evaluation..."
	fuzzi-eval-spam
	@echo ">>> running teacher ensemble evaluation..."
	fuzzi-eval-pate
	@echo ">>> running kmeans evaluation..."
	fuzzi-eval-iris

.PHONY: image
image:
	docker build -t fuzzi-impl:latest -m 8g --squash .
	docker save fuzzi-impl | pigz -9 > fuzzi-artifact.tgz
