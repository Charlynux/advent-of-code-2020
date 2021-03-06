class Node {
  constructor(value) {
    this.value = value;
    this.next = null;
    this.prev = null;
  }
}

class Cups {
  constructor(node) {
    this.head = node;
    this.tail = node;
  }

  append(node) {
    this.tail.next = node;
    node.prev = this.tail;

    this.tail = node;
  }

  draw() {
    const value = this.head.value;

    this.head = this.head.next;
    if (this.head) {
      this.head.prev = null;
    } else {
      this.tail = null;
    }

    return value;
  }

  insertAfter(node, value) {
    const newNode = new Node(value);
    newNode.prev = node;
    newNode.next = node.next;

    if (node.next) {
      node.next.prev = newNode;
    }

    if (this.tail === node) {
      this.tail = newNode;
    }

    node.next = newNode;
  }

  findValue(value) {
    let node = this.head;

    while (node && node.value !== value) {
      node = node.next;
    }

    return node;
  }

  isEmpty() {
    return this.head == null;
  }

  toList() {
    const list = [];

    let node = this.head;
    while (node) {
      list.push(node.value);

      node = node.next;
    }

    return list;
  }

  static fromList([x, ...xs]) {
    const cups = new Cups(new Node(x));

    xs.map((x) => new Node(x)).forEach((node) => cups.append(node));

    return cups;
  }
}

function findDestinationValue(maxValue, current, pickUp) {
  function roundingDec(value) {
    return value > 1 ? value - 1 : maxValue;
  }

  let destinationValue = roundingDec(current);

  while (pickUp.includes(destinationValue)) {
    destinationValue = roundingDec(destinationValue);
  }

  return destinationValue;
}

function createSteps(max) {
  return function step(cups) {
    const current = cups.draw();
    const pickUp = [cups.draw(), cups.draw(), cups.draw()];
    const destinationValue = findDestinationValue(max, current, pickUp);
    const destinationNode = cups.findValue(destinationValue);

    pickUp.reverse().forEach(function (value) {
      cups.insertAfter(destinationNode, value);
    });

    cups.append(new Node(current));

    return cups;
  };
}

function codeLabel(cups) {
  let target = cups.findValue(1);
  let node = target.next;

  let result = "";
  while (node) {
    result += node.value;
    node = node.next;
  }

  node = cups.head;
  while (node !== target) {
    result += node.value;
    node = node.next;
  }

  return result;
}

const stepPart1 = createSteps(9);
function solvePart1(xs) {
  let cups = Cups.fromList(xs);
  for (let index = 0; index < 100; index++) {
    cups = stepPart1(cups);
  }

  return codeLabel(cups);
}

// console.log("Sample : ", solvePart1([3, 8, 9, 1, 2, 5, 4, 6, 7]));
// console.log("Real : ", solvePart1([3, 9, 8, 2, 5, 4, 7, 1, 6]));

function completeCups(cups, max) {
  for (let i = 10; i <= max; i++) {
    cups.append(new Node(i));
  }
}

const maxPart2 = 1000000;
const stepPart2 = createSteps(maxPart2);
function solvePart2(xs) {
  let cups = Cups.fromList(xs);
  completeCups(cups, maxPart2);
  for (let index = 0; index < 10000000; index++) {
    cups = stepPart2(cups);

    if (index % 100 == 0) {
      process.stdout.write(index % 1000 == 0 ? "M" : ".");
    }
  }

  const target = cups.findValue(1);

  return [target.next.value, target.next.next.value];
}

// console.log("Sample : ", solvePart2([3, 8, 9, 1, 2, 5, 4, 6, 7]));
console.log("Real : ", solvePart2([3, 9, 8, 2, 5, 4, 7, 1, 6]));

module.exports = {
  Node,
  Cups,
  createSteps,
};
