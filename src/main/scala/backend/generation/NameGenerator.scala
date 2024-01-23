package backend.generation

class Counter {
	private var value: Int = 0

	def next: Int = {
		value += 1
		value
	}

	def reset(): Unit = value = 0
}

class NameGenerator {
	private val registerCounter: Counter = Counter()
	private val labelCounter: Counter = Counter()

	def nextRegister: String = s"%r.${registerCounter.next}"
	def nextLabel: String = s"L.${labelCounter.next}"

	def reset(): Unit = {
		registerCounter.reset()
		labelCounter.reset()
	}
}
