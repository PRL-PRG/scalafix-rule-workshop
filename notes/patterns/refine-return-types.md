## Refine return types on the fly

This "pattern" is still unclear to me. In any case, it wasn't found in a blogpost, but in several places in the codebase.

### Examples

**Taken from `airbnb/aerosolve`**

Here, I believe they are just passing the ClassTag[T] around 

File `/airlearner-utils/src/main/scala/com/airbnb/common/ml/util/HiveUtil.scala`:

They have only one caller each, shown below. Note how the callers refine T in the return types.

```scala
  def loadDataFromHive[T](
      hiveContext: HiveContext,
      dataQuery: String,
      parseKeyFromHiveRow: (Row) => String,
      parseSampleFromHiveRow: (Row) => T
  )(implicit c: ClassTag[T]):
  RDD[(String, T)] = {
    loadDataFromDataFrame(hiveContext.sql(dataQuery), parseKeyFromHiveRow, parseSampleFromHiveRow)
  }

  def loadDataFromDataFrame[T](
      data: DataFrame,
      parseKeyFromHiveRow: (Row) => String,
      parseSampleFromHiveRow: (Row) => T
  )
    (implicit c: ClassTag[T]): RDD[(String, T)] = {
    data.map(row => {
      val key = parseKeyFromHiveRow(row)
      val t = parseSampleFromHiveRow(row)
      (key, t)
    })
	}
```

File `com/airbnb/common/ml/xgboost/data/ModelData.scala`:

Here it calls the above two functions, and I guess ClassTag is used to specify the type parameters for the return types.
```scala
 def getScoringLabeledPoints(sc: SparkContext,
                              query: String, scoringLabeledPoint: ScoringModelData): RDD[(String, ScoringLabeledPoint)] = {
    val df = ModelData.getDataFrame(sc, query)
    HiveUtil.loadDataFromDataFrame(
      df,
      // score_query_head of scoring.conf also defined S_node_10k_id same as TRAINING_KEY_INDEX
      ModelData.parseKeyFromHiveRow(ModelData.TRAINING_KEY_INDEX),
      scoringLabeledPoint.parseRowToXgboostLabeledPointAndData)
  }

  def getLabeledPointsAndString(sc: SparkContext,
                                query: String, scoringLabeledPoint: ScoringModelData): RDD[(String, Seq[ScoringLabeledPoint])] = {
    val df = ModelData.getDataFrame(sc, query)
    HiveUtil.loadDataFromDataFrameGroupByKey(
      df,
      // score_query_head of scoring.conf also defined S_node_10k_id same as TRAINING_KEY_INDEX
      ModelData.parseKeyFromHiveRow(ModelData.TRAINING_KEY_INDEX),
      scoringLabeledPoint.parseRowToXgboostLabeledPointAndData)
  }
```

**NOTE:** There is another example of just this in the file `com/airbnb/common/ml/strategy/trainer/BinaryRegressionTrainer.scala` of the same project