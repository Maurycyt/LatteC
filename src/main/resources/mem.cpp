#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <cstdint>
#include <cstdlib>
#include <cstdio>

class ReferenceManager {
	int64_t referenceCount{0};

	ReferenceManager() = delete;

protected:
    void * reference{nullptr};

	ReferenceManager(void * reference) : reference(reference) {}

	virtual void freeReference() = 0;

public:
    int64_t getReferenceCount() { return referenceCount; }

	ReferenceManager & operator++(int) {
		referenceCount++;
		return *this;
	}

	ReferenceManager & operator--(int) {
		referenceCount--;
		return *this;
	}

	virtual ~ReferenceManager() = default;
};

std::unordered_map<void *, ReferenceManager *> referenceMap{};
std::unordered_map<int64_t, std::unordered_set<int64_t>> pointerOffsetsInObjects;
std::unordered_set<void *> unboundPointers;

class StringReferenceManager : public ReferenceManager {
protected:
	void freeReference() {
//	    printf("\t\tFREEING STRING REFERENCE! %p\n", reference);
		if (!reference)
		    return;

//        printf("\t\t<<%s>>\n", reference);
		free(reference);
	}

	~StringReferenceManager() {
        freeReference();
    }

public:
	StringReferenceManager(void * reference) : ReferenceManager(reference) {}
};

extern "C"
void decreaseRefCount(void * ptr);

class ArrayReferenceManager : public ReferenceManager {
	bool containsPointers{false};

protected:
	void freeReference() {
	    if (!reference)
	        return;

	    if (containsPointers) {
	        size_t length = *((int64_t *)reference - 1);
	        for (int i = 0; i < length; i++)
	            decreaseRefCount(((void * *)reference)[i]);
	    }
//	    printf("\t\tFreeing array %p.\n", reference);
		free((int64_t *)reference - 1);
	}

	~ArrayReferenceManager() {
        freeReference();
    }

public:
	ArrayReferenceManager(void * reference, bool containsPointers) :
	ReferenceManager(reference), containsPointers(containsPointers) {}
};

class ObjectReferenceManager : public ReferenceManager {
    int64_t classID;

protected:
    void freeReference() {
        if (!reference)
            return;

        for (int64_t offset : pointerOffsetsInObjects[classID]) {
            void * * pointerPointer = (void * *)reference + offset;
            decreaseRefCount(*pointerPointer);
        }
        free(reference);
    }

	~ObjectReferenceManager() {
        freeReference();
    }

public:
    ObjectReferenceManager(void * ptr, int64_t classID) : ReferenceManager(ptr), classID(classID) {}
};

extern "C" {
	void registerString(void * ptr) {
	    referenceMap.emplace(ptr, new StringReferenceManager(ptr));
	    unboundPointers.insert(ptr);
//	    printf("Got %d references tracked.\n", referenceMap.size());
	}

	void registerArray(void * ptr, bool containsPointers) {
//	    printf("\t\tRegistering array %p.\n", ptr);
	    referenceMap.emplace(ptr, new ArrayReferenceManager(ptr, containsPointers));
	    unboundPointers.insert(ptr);
	}

	void registerObject(void * ptr, int64_t classID) {
	    referenceMap.emplace(ptr, new ObjectReferenceManager(ptr, classID));
	    unboundPointers.insert(ptr);
	}

	void registerObjectPointerMemberOffset(int64_t classID, int64_t offset) {
        pointerOffsetsInObjects[classID].insert(offset);
	}

	void increaseRefCount(void * ptr) {
	    // We check if the pointer appears in the map because some pointers
	    // are never registered, like string constants.
	    if (!referenceMap.contains(ptr)) return;
//	    printf("\t\t++ %p\n", ptr);
	    (*referenceMap.at(ptr))++;
	    if (referenceMap.at(ptr)->getReferenceCount() > 0)
	        unboundPointers.erase(ptr);
	}

	void decreaseRefCount(void * ptr) {
	    // We check if the pointer is not null because every pointer
	    // value starts with a null pointer which we want to ignore.
	    // We also check for untracked pointers as in increaseRefCount.
	    if (!ptr) return;
	    if (!referenceMap.contains(ptr)) return;
//	    printf("\t\t-- %p\n", ptr);
	    (*referenceMap.at(ptr))--;
	    if (referenceMap.at(ptr)->getReferenceCount() == 0)
	        unboundPointers.insert(ptr);
	}

	void clearUnboundPointers() {
	    while (!unboundPointers.empty()) {
            std::unordered_set<void *> unboundPointersToClear = unboundPointers;
            unboundPointers.clear();
            for (auto ptr : unboundPointersToClear) {
                ReferenceManager * manager = referenceMap.at(ptr);
                referenceMap.erase(ptr);
                delete manager;
            }
	    }
//	    printf("Got %d references tracked after clear.\n", referenceMap.size());
	}
};
