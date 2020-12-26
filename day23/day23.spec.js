const { Node, Cups, step } = require("./day23");

test("Node constructor", () => {
  const node = new Node(3);
  expect(node.value).toBe(3);
});

test("Cups constructor", () => {
  const node = new Node(42);
  const cups = new Cups(node);

  expect(cups.head).toBe(node);
  expect(cups.tail).toBe(node);
});

test("Cups append", () => {
  const node = new Node(42);
  const node2 = new Node(43);
  const cups = new Cups(node);
  cups.append(node2);

  expect(cups.head).toBe(node);
  expect(cups.tail).toBe(node2);

  expect(node.next).toBe(node2);
  expect(node2.prev).toBe(node);
});

test("Cups pick", () => {
  const node = new Node(42);
  const node2 = new Node(43);
  const cups = new Cups(node);
  cups.append(node2);

  expect(cups.draw()).toBe(42);

  expect(cups.head).toBe(node2);
});

test("Cups fromList", () => {
  const cups = Cups.fromList([42, 43, 44]);

  expect(cups.draw()).toBe(42);
  expect(cups.draw()).toBe(43);
  expect(cups.draw()).toBe(44);

  expect(cups.isEmpty()).toBe(true);
});

test("Cups toList", () => {
  const cups = Cups.fromList([42, 43, 44]);

  expect(cups.toList()).toStrictEqual([42, 43, 44]);
});

test("Cups findValue", () => {
  const cups = Cups.fromList([42, 43, 44]);
  const node = cups.findValue(43);

  expect(node.value).toBe(43);
  expect(node.prev).toBe(cups.head);
  expect(node.next).toBe(cups.tail);
});

test("testAfter middle", () => {
  const cups = Cups.fromList([1, 3]);

  cups.insertAfter(cups.head, 2);

  expect(cups.toList()).toStrictEqual([1, 2, 3]);
});

test("testAfter end", () => {
  const cups = Cups.fromList([1, 3]);

  cups.insertAfter(cups.tail, 2);

  expect(cups.toList()).toStrictEqual([1, 3, 2]);
  expect(cups.tail.value).toStrictEqual(2);
});

test("Sample input move 1", () => {
  const result = step(Cups.fromList([3, 8, 9, 1, 2, 5, 4, 6, 7]));

  expect(result.toList()).toStrictEqual([2, 8, 9, 1, 5, 4, 6, 7, 3]);
});
test("Sample input move 2", () => {
  const result = step(Cups.fromList([2, 8, 9, 1, 5, 4, 6, 7, 3]));

  expect(result.toList()).toStrictEqual([5, 4, 6, 7, 8, 9, 1, 3, 2]);
});

test("CodeLabel", () => {
  const label = codeLabel(Cups.fromList([8, 3, 7, 4, 1, 9, 2, 6, 5]));

  expect(label).toBe("92658374");
});
