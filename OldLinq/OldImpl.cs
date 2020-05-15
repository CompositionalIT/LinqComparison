using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;

namespace System.Linq
{
    public static class OldEnumerable
    {
        static Func<TSource, bool> CombinePredicates<TSource>(Func<TSource, bool> predicate1, Func<TSource, bool> predicate2) {
            return x => predicate1(x) && predicate2(x);
        }
        static Func<TSource, TResult> CombineSelectors<TSource, TMiddle, TResult>(Func<TSource, TMiddle> selector1, Func<TMiddle, TResult> selector2) {
            return x => selector2(selector1(x));
        }
        class WhereSelectEnumerableIterator<TSource, TResult> : Iterator<TResult>
        {
            IEnumerable<TSource> source;
            Func<TSource, bool> predicate;
            Func<TSource, TResult> selector;
            IEnumerator<TSource> enumerator;

            public WhereSelectEnumerableIterator(IEnumerable<TSource> source, Func<TSource, bool> predicate, Func<TSource, TResult> selector) {
                this.source = source;
                this.predicate = predicate;
                this.selector = selector;
            }

            public override Iterator<TResult> Clone() {
                return new WhereSelectEnumerableIterator<TSource, TResult>(source, predicate, selector);
            }

            public override void Dispose() {
                if (enumerator is IDisposable) ((IDisposable)enumerator).Dispose();
                enumerator = null;
                base.Dispose();
            }

            public override bool MoveNext() {
                switch (state) {
                    case 1:
                        enumerator = source.GetEnumerator();
                        state = 2;
                        goto case 2;
                    case 2:
                        while (enumerator.MoveNext()) {
                            TSource item = enumerator.Current;
                            if (predicate == null || predicate(item)) {
                                current = selector(item);
                                return true;
                            }
                        }
                        Dispose();
                        break;
                }
                return false;
            }

            public override IEnumerable<TResult2> Select<TResult2>(Func<TResult, TResult2> selector) {
                return new WhereSelectEnumerableIterator<TSource, TResult2>(source, predicate, CombineSelectors(this.selector, selector));
            }

            public override IEnumerable<TResult> Where(Func<TResult, bool> predicate) {
                return new WhereEnumerableIterator<TResult>(this, predicate);
            }
        }
        abstract class Iterator<TSource> : IEnumerable<TSource>, IEnumerator<TSource>
        {
            int threadId;
            internal int state;
            internal TSource current;

            public Iterator() {
                threadId = Thread.CurrentThread.ManagedThreadId;
            }

            public TSource Current {
                get { return current; }
            }

            public abstract Iterator<TSource> Clone();

            public virtual void Dispose() {
                current = default(TSource);
                state = -1;
            }

            public IEnumerator<TSource> GetEnumerator() {
                if (threadId == Thread.CurrentThread.ManagedThreadId && state == 0) {
                    state = 1;
                    return this;
                }
                Iterator<TSource> duplicate = Clone();
                duplicate.state = 1;
                return duplicate;
            }

            public abstract bool MoveNext();

            public abstract IEnumerable<TResult> Select<TResult>(Func<TSource, TResult> selector);

            public abstract IEnumerable<TSource> Where(Func<TSource, bool> predicate);

            object IEnumerator.Current {
                get { return Current; }
            }

            IEnumerator IEnumerable.GetEnumerator() {
                return GetEnumerator();
            }

            void IEnumerator.Reset() {
                throw new NotImplementedException();
            }
        }
        class WhereEnumerableIterator<TSource> : Iterator<TSource>
        {
            IEnumerable<TSource> source;
            Func<TSource, bool> predicate;
            IEnumerator<TSource> enumerator;

            public WhereEnumerableIterator(IEnumerable<TSource> source, Func<TSource, bool> predicate) {
                this.source = source;
                this.predicate = predicate;
            }

            public override Iterator<TSource> Clone() {
                return new WhereEnumerableIterator<TSource>(source, predicate);
            }

            public override void Dispose() {
                if (enumerator is IDisposable) ((IDisposable)enumerator).Dispose();
                enumerator = null;
                base.Dispose();
            }

            public override bool MoveNext() {
                switch (state) {
                    case 1:
                        enumerator = source.GetEnumerator();
                        state = 2;
                        goto case 2;
                    case 2:
                        while (enumerator.MoveNext()) {
                            TSource item = enumerator.Current;
                            if (predicate(item)) {
                                current = item;
                                return true;
                            }
                        }
                        Dispose();
                        break;
                }
                return false;
            }

            public override IEnumerable<TResult> Select<TResult>(Func<TSource, TResult> selector) {
                return new WhereSelectEnumerableIterator<TSource, TResult>(source, predicate, selector);
            }

            public override IEnumerable<TSource> Where(Func<TSource, bool> predicate) {
                return new WhereEnumerableIterator<TSource>(source, CombinePredicates(this.predicate, predicate));
            }
        }
        class WhereArrayIterator<TSource> : Iterator<TSource>
        {
            TSource[] source;
            Func<TSource, bool> predicate;
            int index;

            public WhereArrayIterator(TSource[] source, Func<TSource, bool> predicate) {
                this.source = source;
                this.predicate = predicate;
            }

            public override Iterator<TSource> Clone() {
                return new WhereArrayIterator<TSource>(source, predicate);
            }

            public override bool MoveNext() {
                if (state == 1) {
                    while (index < source.Length) {
                        TSource item = source[index];
                        index++;
                        if (predicate(item)) {
                            current = item;
                            return true;
                        }
                    }
                    Dispose();
                }
                return false;
            }

            public override IEnumerable<TResult> Select<TResult>(Func<TSource, TResult> selector) {
                return new WhereSelectArrayIterator<TSource, TResult>(source, predicate, selector);
            }

            public override IEnumerable<TSource> Where(Func<TSource, bool> predicate) {
                return new WhereArrayIterator<TSource>(source, CombinePredicates(this.predicate, predicate));
            }
        }
        class WhereListIterator<TSource> : Iterator<TSource>
        {
            List<TSource> source;
            Func<TSource, bool> predicate;
            List<TSource>.Enumerator enumerator;

            public WhereListIterator(List<TSource> source, Func<TSource, bool> predicate) {
                this.source = source;
                this.predicate = predicate;
            }

            public override Iterator<TSource> Clone() {
                return new WhereListIterator<TSource>(source, predicate);
            }

            public override bool MoveNext() {
                switch (state) {
                    case 1:
                        enumerator = source.GetEnumerator();
                        state = 2;
                        goto case 2;
                    case 2:
                        while (enumerator.MoveNext()) {
                            TSource item = enumerator.Current;
                            if (predicate(item)) {
                                current = item;
                                return true;
                            }
                        }
                        Dispose();
                        break;
                }
                return false;
            }

            public override IEnumerable<TResult> Select<TResult>(Func<TSource, TResult> selector) {
                return new WhereSelectListIterator<TSource, TResult>(source, predicate, selector);
            }

            public override IEnumerable<TSource> Where(Func<TSource, bool> predicate) {
                return new WhereListIterator<TSource>(source, CombinePredicates(this.predicate, predicate));
            }
        }
        class WhereSelectArrayIterator<TSource, TResult> : Iterator<TResult>
        {
            TSource[] source;
            Func<TSource, bool> predicate;
            Func<TSource, TResult> selector;
            int index;

            public WhereSelectArrayIterator(TSource[] source, Func<TSource, bool> predicate, Func<TSource, TResult> selector) {
                this.source = source;
                this.predicate = predicate;
                this.selector = selector;
            }

            public override Iterator<TResult> Clone() {
                return new WhereSelectArrayIterator<TSource, TResult>(source, predicate, selector);
            }

            public override bool MoveNext() {
                if (state == 1) {
                    while (index < source.Length) {
                        TSource item = source[index];
                        index++;
                        if (predicate == null || predicate(item)) {
                            current = selector(item);
                            return true;
                        }
                    }
                    Dispose();
                }
                return false;
            }

            public override IEnumerable<TResult2> Select<TResult2>(Func<TResult, TResult2> selector) {
                return new WhereSelectArrayIterator<TSource, TResult2>(source, predicate, CombineSelectors(this.selector, selector));
            }

            public override IEnumerable<TResult> Where(Func<TResult, bool> predicate) {
                return new WhereEnumerableIterator<TResult>(this, predicate);
            }
        }
        class WhereSelectListIterator<TSource, TResult> : Iterator<TResult>
        {
            List<TSource> source;
            Func<TSource, bool> predicate;
            Func<TSource, TResult> selector;
            List<TSource>.Enumerator enumerator;

            public WhereSelectListIterator(List<TSource> source, Func<TSource, bool> predicate, Func<TSource, TResult> selector) {
                this.source = source;
                this.predicate = predicate;
                this.selector = selector;
            }

            public override Iterator<TResult> Clone() {
                return new WhereSelectListIterator<TSource, TResult>(source, predicate, selector);
            }

            public override bool MoveNext() {
                switch (state) {
                    case 1:
                        enumerator = source.GetEnumerator();
                        state = 2;
                        goto case 2;
                    case 2:
                        while (enumerator.MoveNext()) {
                            TSource item = enumerator.Current;
                            if (predicate == null || predicate(item)) {
                                current = selector(item);
                                return true;
                            }
                        }
                        Dispose();
                        break;
                }
                return false;
            }

            public override IEnumerable<TResult2> Select<TResult2>(Func<TResult, TResult2> selector) {
                return new WhereSelectListIterator<TSource, TResult2>(source, predicate, CombineSelectors(this.selector, selector));
            }

            public override IEnumerable<TResult> Where(Func<TResult, bool> predicate) {
                return new WhereEnumerableIterator<TResult>(this, predicate);
            }
        }
        internal abstract class EnumerableSorter<TElement>
        {
            internal abstract void ComputeKeys(TElement[] elements, int count);

            internal abstract int CompareKeys(int index1, int index2);

            internal int[] Sort(TElement[] elements, int count) {
                ComputeKeys(elements, count);
                int[] map = new int[count];
                for (int i = 0; i < count; i++) map[i] = i;
                QuickSort(map, 0, count - 1);
                return map;
            }

            void QuickSort(int[] map, int left, int right) {
                do {
                    int i = left;
                    int j = right;
                    int x = map[i + ((j - i) >> 1)];
                    do {
                        while (i < map.Length && CompareKeys(x, map[i]) > 0) i++;
                        while (j >= 0 && CompareKeys(x, map[j]) < 0) j--;
                        if (i > j) break;
                        if (i < j) {
                            int temp = map[i];
                            map[i] = map[j];
                            map[j] = temp;
                        }
                        i++;
                        j--;
                    } while (i <= j);
                    if (j - left <= right - i) {
                        if (left < j) QuickSort(map, left, j);
                        left = i;
                    }
                    else {
                        if (i < right) QuickSort(map, i, right);
                        right = j;
                    }
                } while (left < right);
            }
        }
        internal class EnumerableSorter<TElement, TKey> : EnumerableSorter<TElement>
        {
            internal Func<TElement, TKey> keySelector;
            internal IComparer<TKey> comparer;
            internal bool descending;
            internal EnumerableSorter<TElement> next;
            internal TKey[] keys;

            internal EnumerableSorter(Func<TElement, TKey> keySelector, IComparer<TKey> comparer, bool descending, EnumerableSorter<TElement> next) {
                this.keySelector = keySelector;
                this.comparer = comparer;
                this.descending = descending;
                this.next = next;
            }

            internal override void ComputeKeys(TElement[] elements, int count) {
                keys = new TKey[count];
                for (int i = 0; i < count; i++) keys[i] = keySelector(elements[i]);
                if (next != null) next.ComputeKeys(elements, count);
            }

            internal override int CompareKeys(int index1, int index2) {
                int c = comparer.Compare(keys[index1], keys[index2]);
                if (c == 0) {
                    if (next == null) return index1 - index2;
                    return next.CompareKeys(index1, index2);
                }
                return descending ? -c : c;
            }
        }
        struct Buffer<TElement>
        {
            internal TElement[] items;
            internal int count;

            internal Buffer(IEnumerable<TElement> source) {
                TElement[] items = null;
                int count = 0;
                ICollection<TElement> collection = source as ICollection<TElement>;
                if (collection != null) {
                    count = collection.Count;
                    if (count > 0) {
                        items = new TElement[count];
                        collection.CopyTo(items, 0);
                    }
                }
                else {
                    foreach (TElement item in source) {
                        if (items == null) {
                            items = new TElement[4];
                        }
                        else if (items.Length == count) {
                            TElement[] newItems = new TElement[checked(count * 2)];
                            Array.Copy(items, 0, newItems, 0, count);
                            items = newItems;
                        }
                        items[count] = item;
                        count++;
                    }
                }
                this.items = items;
                this.count = count;
            }

            internal TElement[] ToArray() {
                if (count == 0) return new TElement[0];
                if (items.Length == count) return items;
                TElement[] result = new TElement[count];
                Array.Copy(items, 0, result, 0, count);
                return result;
            }
        }
        internal abstract class OrderedEnumerable<TElement> : IOrderedEnumerable<TElement>
        {
            internal IEnumerable<TElement> source;

            public IEnumerator<TElement> GetEnumerator() {
                Buffer<TElement> buffer = new Buffer<TElement>(source);
                if (buffer.count > 0) {
                    EnumerableSorter<TElement> sorter = GetEnumerableSorter(null);
                    int[] map = sorter.Sort(buffer.items, buffer.count);
                    sorter = null;
                    for (int i = 0; i < buffer.count; i++) yield return buffer.items[map[i]];
                }
            }

            internal abstract EnumerableSorter<TElement> GetEnumerableSorter(EnumerableSorter<TElement> next);

            IEnumerator IEnumerable.GetEnumerator() {
                return GetEnumerator();
            }

            IOrderedEnumerable<TElement> IOrderedEnumerable<TElement>.CreateOrderedEnumerable<TKey>(Func<TElement, TKey> keySelector, IComparer<TKey> comparer, bool descending) {
                OrderedEnumerable<TElement, TKey> result = new OrderedEnumerable<TElement, TKey>(source, keySelector, comparer, descending);
                result.parent = this;
                return result;
            }
        }
        internal class OrderedEnumerable<TElement, TKey> : OrderedEnumerable<TElement>
        {
            internal OrderedEnumerable<TElement> parent;
            internal Func<TElement, TKey> keySelector;
            internal IComparer<TKey> comparer;
            internal bool descending;

            internal OrderedEnumerable(IEnumerable<TElement> source, Func<TElement, TKey> keySelector, IComparer<TKey> comparer, bool descending) {
                if (source == null) throw new ArgumentNullException("source");
                if (keySelector == null) throw new ArgumentNullException("keySelector");
                this.source = source;
                this.parent = null;
                this.keySelector = keySelector;
                this.comparer = comparer != null ? comparer : Comparer<TKey>.Default;
                this.descending = descending;
            }

            internal override EnumerableSorter<TElement> GetEnumerableSorter(EnumerableSorter<TElement> next) {
                EnumerableSorter<TElement> sorter = new EnumerableSorter<TElement, TKey>(keySelector, comparer, descending, next);
                if (parent != null) sorter = parent.GetEnumerableSorter(sorter);
                return sorter;
            }
        }

        public static TSource LastOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new ArgumentNullException("source");
            if (predicate == null) throw new ArgumentNullException("predicate");
            TSource result = default(TSource);
            foreach (TSource element in source) {
                if (predicate(element)) {
                    result = element;
                }
            }
            return result;
        }
        public static TSource FirstOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new Exception("source");
            if (predicate == null) throw new Exception("predicate");
            foreach (TSource element in source) {
                if (predicate(element)) return element;
            }
            return default(TSource);
        }
        public static TSource SingleOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new Exception("source");
            if (predicate == null) throw new Exception("predicate");
            TSource result = default(TSource);
            long count = 0;
            foreach (TSource element in source) {
                if (predicate(element)) {
                    result = element;
                    checked { count++; }
                }
            }
            switch (count) {
                case 0: return default(TSource);
                case 1: return result;
            }
            throw new Exception();
        }
        public static int Count<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new Exception("source");
            if (predicate == null) throw new Exception("predicate");
            int count = 0;
            foreach (TSource element in source) {
                checked {
                    if (predicate(element)) count++;
                }
            }
            return count;
        }

        public static IEnumerable<TResult> Select<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector) {
            if (source == null) throw new ArgumentNullException("source");
            if (selector == null) throw new ArgumentNullException("selector");
            if (source is Iterator<TSource>) return ((Iterator<TSource>)source).Select(selector);
            if (source is TSource[]) return new WhereSelectArrayIterator<TSource, TResult>((TSource[])source, null, selector);
            if (source is List<TSource>) return new WhereSelectListIterator<TSource, TResult>((List<TSource>)source, null, selector);
            return new WhereSelectEnumerableIterator<TSource, TResult>(source, null, selector);
        }
        public static IEnumerable<TSource> Where<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate) {
            if (source == null) throw new ArgumentNullException("source");
            if (predicate == null) throw new ArgumentNullException("predicate");
            if (source is Iterator<TSource>) return ((Iterator<TSource>)source).Where(predicate);
            if (source is TSource[]) return new WhereArrayIterator<TSource>((TSource[])source, predicate);
            if (source is List<TSource>) return new WhereListIterator<TSource>((List<TSource>)source, predicate);
            return new WhereEnumerableIterator<TSource>(source, predicate);
        }
        public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector) {
            return new OrderedEnumerable<TSource, TKey>(source, keySelector, null, false);
        }
    }
}
